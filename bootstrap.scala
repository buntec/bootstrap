//> using toolkit typelevel:latest

package bootstrap

import cats.effect._
import cats.effect.implicits._
import cats._
import cats.syntax.all._
import fs2.Stream
import fs2.io
import fs2.io.process.*
import scala.util.control.NoStackTrace
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.io.file.PosixPermissions
import fs2.io.file.PosixPermission
import org.http4s.Uri
import java.nio.file.InvalidPathException
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled

import scala.io.AnsiColor

trait Bootstrap[F[_]] {

  def logInfo(msg: String): F[Unit]

  def logError(msg: String): F[Unit]

  def logWarning(msg: String): F[Unit]

  def pathFromString(s: String): F[Path]

  def pathExists(path: Path): F[Boolean]

  // equivalent to chmod +x
  def makeExecutable(path: Path): F[Unit]

  def ensureCommand(cmd: String): F[Unit]

  // returns path to downloaded (possibly uncompressed) file or folder
  def downloadFile(
      url: String,
      outDir: Path,
      extract: Boolean = true,
      renameTo: Option[String] = None
  ): F[Path]

  def cloneRepo(
      repository: String,
      destination: Option[Path] = None
  ): F[Unit]

}

case class NonZeroExitCodeException(code: Int) extends NoStackTrace

case class MissingCommandExecutable(override val getMessage: String)
    extends NoStackTrace

object syntax {

  extension [F[_]: Concurrent: Files: std.Console](process: Process[F]) {

    private def logInfo(msg: String) =
      std.Console[F].println(s"${AnsiColor.GREEN}$msg${AnsiColor.RESET}")

    private def logError(msg: String) =
      std.Console[F].println(s"${AnsiColor.RED}$msg${AnsiColor.RESET}")

    def logStdout: F[Unit] = process.stdout
      .through(fs2.text.utf8Decode)
      .through(fs2.text.lines)
      .evalMap(logInfo)
      .compile
      .drain

    def logErrors: F[Unit] = process.stderr
      .through(fs2.text.utf8Decode)
      .through(fs2.text.lines)
      .evalMap(logError)
      .compile
      .drain

    def writeStdoutToFile(path: Path): F[Unit] =
      process.stdout.through(Files[F].writeAll(path)).compile.drain

    def raiseOnNonZeroExitCode = process.exitValue.flatMap(n =>
      ApplicativeThrow[F].raiseUnless(n == 0)(NonZeroExitCodeException(n))
    )

  }

}

object Bootstrap {

  def apply[F[_]](implicit ev: Bootstrap[F]) = ev

  implicit def bootstrapForIO: Bootstrap[IO] = create[IO]

  def create[F[_]: Temporal: Files: Processes: std.Console]: Bootstrap[F] =
    new Bootstrap[F] {

      import syntax._

      def logInfo(msg: String) =
        std.Console[F].println(s"${AnsiColor.GREEN}$msg${AnsiColor.RESET}")

      def logWarning(msg: String): F[Unit] =
        std.Console[F].println(s"${AnsiColor.YELLOW}$msg${AnsiColor.RESET}")

      def logError(msg: String) =
        std.Console[F].println(s"${AnsiColor.RED}$msg${AnsiColor.RESET}")

      private def withLogging[A](fa: F[A])(label: String) =
        Temporal[F].guaranteeCase(logInfo(s"$label...") *> fa) {
          _ match
            case Succeeded(_) => logInfo(s"$label completed successfully...")
            case Errored(e)   => logError(s"$label failed with errors: $e")
            case Canceled()   => logWarning(s"$label was cancelled")
        }

      def pathFromString(s: String): F[Path] =
        ApplicativeThrow[F].catchOnly[InvalidPathException](Path(s))

      def pathExists(path: Path): F[Boolean] =
        Files[F].exists(path)

      override def makeExecutable(path: Path): F[Unit] =
        Files[F].getPosixPermissions(path).flatMap { perm =>
          Files[F].setPosixPermissions(
            path,
            PosixPermissions
              .fromInt(
                perm.value |
                  PosixPermission.OwnerExecute.value |
                  PosixPermission.GroupExecute.value |
                  PosixPermission.OthersExecute.value
              )
              .get // can't fail!
          )
        }

      def ensureCommand(cmd: String): F[Unit] =
        ProcessBuilder("which", cmd).spawn.use(proc =>
          proc.exitValue.flatMap(n =>
            ApplicativeThrow[F].raiseUnless(n == 0)(
              MissingCommandExecutable(s"missing executable: $cmd")
            )
          )
        )

      def downloadFile(
          url: String,
          outDir: Path,
          extract: Boolean = true,
          renameTo: Option[String] = None
      ): F[Path] =
        withLogging(
          ensureCommand("curl") *>
            Files[F].currentWorkingDirectory
              .map(cwd => cwd.resolve(outDir).absolute)
              .flatMap { absOutDir =>
                Files[F].createDirectories(outDir) *>
                  ProcessBuilder("curl", "-L", url)
                    .withWorkingDirectory(outDir)
                    .spawn
                    .use { curl =>
                      ApplicativeThrow[F]
                        .fromOption(
                          url.split("/").lastOption,
                          new IllegalArgumentException(
                            s"the url $url doesn't seem to point to a file"
                          )
                        )
                        .flatMap { fileName =>
                          (if (!extract) {
                             curl.stdout
                               .through(Files[F].writeAll(outDir / fileName))
                               .compile
                               .drain
                               .as((absOutDir / fileName).absolute)
                           } else if (
                             fileName.endsWith(".tar.gz") || url
                               .endsWith(".tgz")
                           ) {
                             ProcessBuilder("tar", "zx")
                               .withWorkingDirectory(outDir)
                               .spawn
                               .use { tar =>
                                 val outPath =
                                   absOutDir / (if (
                                                  fileName.endsWith(".tar.gz")
                                                )
                                                  fileName.dropRight(7)
                                                else fileName.dropRight(4))
                                 (
                                   curl.raiseOnNonZeroExitCode,
                                   curl.logErrors,
                                   tar.logErrors,
                                   tar.raiseOnNonZeroExitCode,
                                   curl.stdout.through(tar.stdin).compile.drain
                                 ).parTupled.as(outPath)
                               }
                           } else if (fileName.endsWith(".gz")) {
                             ProcessBuilder("gzip", "-d")
                               .withWorkingDirectory(outDir)
                               .spawn
                               .use { gzip =>
                                 val outPath = absOutDir / fileName.dropRight(3)
                                 (
                                   curl.logErrors,
                                   curl.raiseOnNonZeroExitCode,
                                   gzip.logErrors,
                                   gzip.raiseOnNonZeroExitCode,
                                   curl.stdout
                                     .through(gzip.stdin)
                                     .compile
                                     .drain,
                                   gzip.writeStdoutToFile(outPath)
                                 ).parTupled.as(outPath)
                               }
                           } else if (fileName.endsWith(".zip")) {
                             ProcessBuilder("unzip", "-d")
                               .withWorkingDirectory(outDir)
                               .spawn
                               .use { unzip =>
                                 val outPath = absOutDir / fileName.dropRight(4)
                                 (
                                   curl.logErrors,
                                   curl.raiseOnNonZeroExitCode,
                                   unzip.logErrors,
                                   unzip.raiseOnNonZeroExitCode,
                                   curl.stdout
                                     .through(unzip.stdin)
                                     .compile
                                     .drain,
                                   unzip.writeStdoutToFile(outPath)
                                 ).parTupled.as(outPath)
                               }
                           } else {
                             ApplicativeThrow[F].raiseError(
                               new Exception(
                                 s"Failed to recognize compression format from url $url"
                               )
                             )
                           })
                        }
                    }
                    .flatMap { outFile =>
                      renameTo.fold(outFile.pure[F]) { newFileName =>
                        val newOut = absOutDir / newFileName
                        Files[F].move(outFile, newOut).as(newOut)
                      }
                    }
              }
        )(s"downloading $url, writing result to $outDir")

      def cloneRepo(
          repository: String,
          destination: Option[Path] = None
      ): F[Unit] =
        withLogging(
          ensureCommand("git") *>
            destination
              .fold(ProcessBuilder("git", "clone", repository))(out =>
                ProcessBuilder("git", "clone", repository, out.toString)
              )
              .spawn
              .use(proc =>
                (proc.logErrors, proc.raiseOnNonZeroExitCode).parTupled.void
              )
        )(
          destination.fold(s"Cloning git repo $repository")(dest =>
            f"Cloning git repo $repository to $dest"
          )
        )

    }

}

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
import concurrent.duration.*

import scala.io.AnsiColor

trait Bootstrap[F[_]] {

  def logInfo(msg: String): F[Unit]

  def logError(msg: String): F[Unit]

  def logWarning(msg: String): F[Unit]

  /** Tries to create a `fs2.io.file.Path` from a string. */
  def pathFromString(s: String): F[Path]

  /** Returns true if the specified path exists. */
  def pathExists(path: Path): F[Boolean]

  /** Creates the specified directory and any non-existant parent directories.
    */
  def createDirs(path: Path): F[Unit]

  /** Equivalent to `chmod +x`. */
  def makeExecutable(path: Path): F[Unit]

  /** Raises in `F` if `cmd` doesn't exist. */
  def ensureCommand(cmd: String): F[Unit]

  /** Writes the given string to a file at the given path. */
  def writeConfigFile(dest: Path, content: String): F[Unit]

  /** Executes a shell script (from `workingDir` if provided) and returns the
    * exit code.
    */
  def executeShellScript(
      script: String,
      executable: String,
      workingDir: Option[Path] = None
  ): F[Int]

  /** Executes a bash script (from `workingDir` if provided) and returns the
    * exit code.
    */
  def executeBashScript(script: String, workingDir: Option[Path] = None): F[Int]

  /** Executes a fish script (from `workingDir` if provided) and returns the
    * exit code.
    */
  def executeFishScript(script: String, workingDir: Option[Path] = None): F[Int]

  /** Attempts to download the file at the given URL, decompresses it if
    * `extract` is `true` and returns the path to the result.
    */
  def downloadFile(
      url: String,
      outDir: Path,
      extract: Boolean = true,
      renameTo: Option[String] = None
  ): F[Path]

  /** Clones a git repo. */
  def cloneRepo(
      repository: String,
      destination: Option[Path] = None,
      extraArgs: List[String] = Nil
  ): F[Unit]

}

case class NonZeroExitCodeException(code: Int) extends NoStackTrace

case class MissingCommandExecutable(override val getMessage: String)
    extends NoStackTrace

object syntax {

  extension [F[_]: Concurrent: Files: std.Console](process: Process[F]) {

    private def logInfo(msg: String) = std.Console[F].println(msg)

    private def logError(msg: String) = std.Console[F].println(msg)

    def logStdout: F[Unit] = process.stdout
      .through(fs2.text.utf8Decode)
      .through(fs2.text.lines)
      .evalMap(logInfo)
      .compile
      .drain

    def logStderr: F[Unit] = process.stderr
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

      private val F = Temporal[F]

      import syntax._

      override def logInfo(msg: String) =
        std.Console[F].println(s"${AnsiColor.GREEN}$msg${AnsiColor.RESET}")

      override def logWarning(msg: String): F[Unit] =
        std.Console[F].println(s"${AnsiColor.YELLOW}$msg${AnsiColor.RESET}")

      override def logError(msg: String) =
        std.Console[F].println(s"${AnsiColor.RED}$msg${AnsiColor.RESET}")

      private def withLogging[A](fa: F[A])(label: String) =
        Stream
          .eval(F.guaranteeCase(logInfo(s"$label...") *> fa) {
            _ match
              case Succeeded(_) => logInfo(s"✅ - $label")
              case Errored(e)   => logError(s"❗ - $label failed: $e")
              case Canceled()   => logWarning(s"🛑 - $label was cancelled")
          })
          .concurrently {
            Stream
              .awakeDelay(3.seconds)
              .evalMap(d => logInfo(s"⌚️ (${d.toSeconds} secs) - $label"))
          }
          .compile
          .lastOrError

      override def pathFromString(s: String): F[Path] =
        ApplicativeThrow[F].catchOnly[InvalidPathException](Path(s))

      override def pathExists(path: Path): F[Boolean] =
        Files[F].exists(path)

      override def createDirs(path: Path): F[Unit] =
        Files[F].createDirectories(path)

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

      override def writeConfigFile(dest: Path, content: String): F[Unit] =
        withLogging(
          Stream
            .emit(content)
            .covary[F]
            .through(Files[F].writeUtf8(dest))
            .compile
            .drain
        )(s"writing $dest")

      override def executeShellScript(
          script: String,
          executable: String,
          workingDir: Option[Path]
      ): F[Int] =
        withLogging(
          Files[F].tempDirectory.use { tmpDir =>
            val scriptPath = tmpDir / "script.sh"
            Stream
              .emit(script)
              .covary[F]
              .through(Files[F].writeUtf8(scriptPath))
              .compile
              .drain *> {
              val pb = ProcessBuilder(executable, s"$scriptPath")
              workingDir
                .fold(pb)(wd => pb.withWorkingDirectory(wd))
                .spawn
                .use(proc =>
                  (proc.logStderr, proc.logStdout, proc.exitValue).parTupled
                    .map((_._3))
                )
            }
          }
        )(s"running shell ($executable) script '${script}'")

      override def executeBashScript(
          script: String,
          workingDir: Option[Path]
      ): F[Int] = executeShellScript(script, "/bin/bash", workingDir)

      override def executeFishScript(
          script: String,
          workingDir: Option[Path]
      ): F[Int] = executeShellScript(script, "/bin/fish", workingDir)

      override def downloadFile(
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
                                   curl.logStderr,
                                   tar.logStderr,
                                   tar.raiseOnNonZeroExitCode,
                                   curl.stdout.through(tar.stdin).compile.drain
                                 ).parTupled.as(outPath)
                               }
                           } else if (fileName.endsWith(".tar.xz")) {
                             ProcessBuilder("tar", "-xJ")
                               .withWorkingDirectory(outDir)
                               .spawn
                               .use { tar =>
                                 val outPath =
                                   absOutDir / fileName.dropRight(7)
                                 (
                                   curl.raiseOnNonZeroExitCode,
                                   curl.logStderr,
                                   tar.logStderr,
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
                                   curl.logStderr,
                                   curl.raiseOnNonZeroExitCode,
                                   gzip.logStderr,
                                   gzip.raiseOnNonZeroExitCode,
                                   curl.stdout
                                     .through(gzip.stdin)
                                     .compile
                                     .drain,
                                   gzip.writeStdoutToFile(outPath)
                                 ).parTupled.as(outPath)
                               }
                           } else if (fileName.endsWith(".zip")) {
                             Files[F].tempDirectory.use { tmpDir =>
                               val tmpFile = tmpDir / fileName
                               curl.stdout
                                 .through(Files[F].writeAll(tmpFile))
                                 .compile
                                 .drain
                                 .flatMap { _ =>
                                   ProcessBuilder(
                                     "unzip",
                                     s"$tmpFile"
                                   ).withWorkingDirectory(outDir)
                                     .spawn
                                     .use { unzip =>
                                       val outPath =
                                         absOutDir / fileName.dropRight(4)
                                       (
                                         curl.logStderr,
                                         curl.raiseOnNonZeroExitCode,
                                         unzip.logStderr,
                                         unzip.raiseOnNonZeroExitCode,
                                         unzip.logStderr,
                                         unzip.logStdout
                                       ).parTupled.as(absOutDir)
                                     }
                                 }
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

      override def cloneRepo(
          repository: String,
          destination: Option[Path] = None,
          extraArgs: List[String] = Nil
      ): F[Unit] =
        withLogging(
          ensureCommand("git") *>
            destination
              .fold(
                ProcessBuilder("git", "clone" :: extraArgs ::: List(repository))
              )(out =>
                ProcessBuilder(
                  "git",
                  "clone" :: extraArgs ::: List(repository, out.toString)
                )
              )
              .spawn
              .use(proc =>
                (
                  proc.logStdout,
                  proc.logStderr,
                  proc.raiseOnNonZeroExitCode
                ).parTupled.void
              )
        )(
          destination.fold(s"cloning git repo $repository")(dest =>
            f"cloning git repo $repository to $dest"
          )
        )

    }

}

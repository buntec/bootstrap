//> using toolkit typelevel:latest

package bootstrap

import cats.effect._
import cats._
import cats.syntax.all._
import fs2.Stream
import fs2.io
import fs2.io.process.*
import scala.util.control.NoStackTrace
import fs2.io.file.Files
import fs2.io.file.Path
import org.http4s.Uri
import java.nio.file.InvalidPathException
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled

trait Bootstrap[F[_]] {

  def pathFromString(s: String): F[Path]

  def pathExists(path: Path): F[Boolean]

  def ensureCommand(cmd: String): F[Unit]

  def downloadFile(
      url: String,
      outDir: Path,
      extract: Boolean = true
  ): F[Unit]

  def cloneRepo(
      repository: String,
      destination: Option[Path] = None
  ): F[Unit]

}

case class NonZeroExitCodeException(code: Int, stderr: String)
    extends NoStackTrace

case class MissingCommandExecutable(override val getMessage: String)
    extends NoStackTrace

object Bootstrap {

  def apply[F[_]](implicit ev: Bootstrap[F]) = ev

  implicit def bootstrapForIO: Bootstrap[IO] = create[IO]

  def create[F[_]: Temporal: Files: Processes: std.Console] = new Bootstrap[F] {

    private def info(msg: String) = std.Console[F].println(msg)

    private def error(msg: String) = std.Console[F].println(msg)

    private def withLogging[A](fa: F[A])(label: String) =
      Temporal[F].guaranteeCase(info(s"step: $label...") *> fa) {
        _ match
          case Succeeded(_) => info(s"$label completed successfully...")
          case Errored(e)   => error(s"$label failed with errors: $e")
          case Canceled()   => info(s"$label was cancelled")
      }

    def pathFromString(s: String): F[Path] =
      ApplicativeThrow[F].catchOnly[InvalidPathException](Path(s))

    def pathExists(path: Path): F[Boolean] =
      Files[F].exists(path)

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
        extract: Boolean = true
    ): F[Unit] =
      withLogging(
        ensureCommand("curl") *>
          Files[F].createDirectories(outDir) *>
          ProcessBuilder("curl", "-L", url)
            .withWorkingDirectory(outDir)
            .spawn
            .use { curl =>
              ApplicativeThrow[F]
                .fromOption(
                  url.split("/").lastOption,
                  new IllegalArgumentException(
                    s"the url $url doesn't point to a file"
                  )
                )
                .flatMap(pathFromString)
                .flatMap { outPath =>
                  (if (!extract) {
                     curl.stdout
                       .through(Files[F].writeAll(outPath))
                       .compile
                       .drain
                   } else if (url.endsWith(".tar.gz") || url.endsWith(".tgz")) {
                     ProcessBuilder("tar", "zx")
                       .withWorkingDirectory(outDir)
                       .spawn
                       .use(tar => curl.stdout.through(tar.stdin).compile.drain)
                   } else if (url.endsWith(".gz")) {
                     ProcessBuilder("gzip", "-d", outPath.toString)
                       .withWorkingDirectory(outDir)
                       .spawn
                       .use(gzip =>
                         curl.stdout.through(gzip.stdin).compile.drain
                       )
                   } else if (url.endsWith(".zip")) {
                     ProcessBuilder("unzip", "-d", outPath.toString)
                       .withWorkingDirectory(outDir)
                       .spawn
                       .use(gzip =>
                         curl.stdout.through(gzip.stdin).compile.drain
                       )
                   } else {
                     ApplicativeThrow[F].raiseError(
                       new Exception("Failed to recognize compression format")
                     )
                   })
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
            .use { proc =>
              proc.exitValue.flatMap {
                case 0 => Applicative[F].unit
                case n =>
                  proc.stderr
                    .through(fs2.text.utf8Decode)
                    .compile
                    .string
                    .flatMap { stderr =>
                      ApplicativeThrow[F]
                        .raiseError(NonZeroExitCodeException(n, stderr))
                    }
              }
            }
      )(
        destination.fold(s"Cloning git repo $repository")(dest =>
          f"Cloning git repo $repository to $dest"
        )
      )

  }

}

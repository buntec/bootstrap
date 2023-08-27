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

case class NonZeroExitCodeException(code: Int, stderr: String)
    extends NoStackTrace

case class MissingCommandExecutable(override val getMessage: String)
    extends NoStackTrace

def pathFromString[F[_]: ApplicativeThrow](s: String): F[Path] =
  ApplicativeThrow[F].catchOnly[InvalidPathException](Path(s))

def pathExists[F[_]: Concurrent: Files](path: Path): F[Boolean] =
  Files[F].exists(path)

def ensureCommand[F[_]: Concurrent: Processes](cmd: String): F[Unit] =
  ProcessBuilder("which", cmd).spawn.use(proc =>
    proc.exitValue.flatMap(n =>
      ApplicativeThrow[F].raiseUnless(n == 0)(
        MissingCommandExecutable(s"missing executable: $cmd")
      )
    )
  )

def downloadFile[F[_]: Concurrent: Processes: Files](
    url: String,
    outDir: Path,
    extract: Boolean = true
): F[Unit] =
  ensureCommand("curl") *>
    Files[F].createDirectories(outDir) *>
    ProcessBuilder("curl", "-L", url)
      .withWorkingDirectory(outDir)
      .spawn
      .use { curl =>
        ApplicativeThrow[F]
          .fromOption(
            url.split("/").lastOption,
            new IllegalArgumentException("the url doesn't point to a file")
          )
          .flatMap(pathFromString)
          .flatMap { outPath =>
            (if (!extract) {
               curl.stdout.through(Files[F].writeAll(outPath)).compile.drain
             } else if (url.endsWith(".tar.gz") || url.endsWith(".tgz")) {
               ProcessBuilder("tar", "zx").spawn
                 .use(tar => curl.stdout.through(tar.stdin).compile.drain)
             } else if (url.endsWith(".gz")) {
               ProcessBuilder("gzip", "-d", outPath.toString).spawn
                 .use(gzip => curl.stdout.through(gzip.stdin).compile.drain)
             } else if (url.endsWith(".zip")) {
               ProcessBuilder("unzip", "-d", outPath.toString).spawn
                 .use(gzip => curl.stdout.through(gzip.stdin).compile.drain)
             } else {
               ApplicativeThrow[F].raiseError(
                 new Exception("Failed to recognize compression format")
               )
             })
          }
      }

def cloneRepo[F[_]: Concurrent: Processes](
    repository: String,
    destination: Option[Path] = None
): F[Unit] =
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
            proc.stderr.through(fs2.text.utf8Decode).compile.string.flatMap {
              stderr =>
                ApplicativeThrow[F]
                  .raiseError(NonZeroExitCodeException(n, stderr))
            }
        }
      }

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

case class NonZeroExitCodeException(code: Int, stderr: String)
    extends NoStackTrace

object os {

  def pathExists[F[_]: Concurrent: Files](path: String): F[Boolean] =
    Files[F].exists(Path(path))

}

def download[F[_]: Concurrent: Processes: Files](
    url: String,
    destination: String,
    extract: Boolean
): F[Unit] =
  ProcessBuilder("curl", "-L", url).spawn.use(curl =>
    if (!extract) {
      curl.stdout.through(Files[F].writeAll(Path(destination))).compile.drain
    } else if (url.endsWith(".tar.gz") || url.endsWith(".tgz")) {
      ProcessBuilder("tar", "zx", "-C", destination).spawn.use(tar =>
        curl.stdout.through(tar.stdin).compile.drain
      )
    } else if (url.endsWith(".gz")) {
      ProcessBuilder("gzip", "-d", destination).spawn.use(gzip =>
        curl.stdout.through(gzip.stdin).compile.drain
      )
    } else {
      ApplicativeThrow[F].raiseError(
        new Exception("Failed to recognize compression format")
      )
    }
  )

object git {

  def clone[F[_]: Concurrent: Processes](repository: String): F[Unit] =
    ProcessBuilder("git", "clone", repository).spawn.use { proc =>
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

}

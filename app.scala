import cats.effect._
import cats._
import cats.syntax.all._
import fs2.Stream
import fs2.io
import fs2.io.process.*
import scala.util.control.NoStackTrace
import fs2.io.file.Files
import fs2.io.file.Path

import bootstrap as bs

object Main extends IOApp.Simple {

  def runF[F[_]: Concurrent: Files: Processes] = for {

    _ <- bs.pathFromString[F]("./tmp/neovim").flatMap { dest =>
      bs.downloadFile[F](
        "https://github.com/neovim/neovim/releases/download/v0.9.1/nvim-macos.tar.gz",
        dest
      )
    }

  } yield ()

  override def run: IO[Unit] = runF[IO]

}

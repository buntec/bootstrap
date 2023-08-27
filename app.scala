import cats.effect._
import cats._
import cats.syntax.all._
import fs2.Stream
import fs2.io
import fs2.io.process.*
import scala.util.control.NoStackTrace
import fs2.io.file.Files
import fs2.io.file.Path

import cats.effect.std.UUIDGen

object Main extends IOApp.Simple {

  object V {
    val nvim = "0.4.1"
    val sbt = "1.9.4"
  }

  def runF[F[_]: MonadThrow: bootstrap.Bootstrap: UUIDGen: std.Console] = for {

    uuid <- UUIDGen[F].randomUUID

    bs = bootstrap.Bootstrap[F]

    workDir <- bs.pathFromString(s"./workspace-$uuid")

    _ <- bs.downloadFile(
      s"https://github.com/neovim/neovim/releases/download/v${V.nvim}/nvim-macos.tar.gz",
      workDir / "neovim"
    )

    _ <- bs.downloadFile(
      "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz",
      workDir / "coursier"
    )

    _ <- bs.downloadFile(
      "https://github.com/Virtuslab/scala-cli/releases/latest/download/scala-cli-x86_64-pc-linux.gz",
      workDir / "scala-cli"
    )

    _ <- bs.downloadFile(
      s"https://github.com/sbt/sbt/releases/download/v${V.sbt}/sbt-${V.sbt}.tgz",
      workDir / "sbt"
    )

  } yield ()

  override def run: IO[Unit] = runF[IO]

}

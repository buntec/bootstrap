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

import cats.effect.std.UUIDGen

object Main extends IOApp.Simple {

  object V {
    val nvim = "0.9.1"
    val sbt = "1.9.4"
    val nodejs = "18.17.1"
  }

  def runF[
      F[_]: Concurrent: bootstrap.Bootstrap: Files: UUIDGen: std.Console
  ]: F[Unit] = {

    val bs = bootstrap.Bootstrap[F]

    UUIDGen[F].randomUUID.flatMap { uuid =>
      bs.pathFromString(s"./workspace-$uuid")
        .flatTap(bs.createDirs)
        .flatMap { workDir =>

          val installNeovim = bs.downloadFile(
            s"https://github.com/neovim/neovim/releases/download/v${V.nvim}/nvim-macos.tar.gz",
            workDir,
            renameTo = "neovim".some
          )

          val installCoursier = bs
            .downloadFile(
              "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz",
              workDir,
              renameTo = "cs".some
            )
            .flatMap(bs.makeExecutable)

          val installScalaCli = bs
            .downloadFile(
              "https://github.com/Virtuslab/scala-cli/releases/latest/download/scala-cli-x86_64-pc-linux.gz",
              workDir,
              renameTo = "scala-cli".some
            )
            .flatMap(bs.makeExecutable)

          val installSbt = bs.downloadFile(
            s"https://github.com/sbt/sbt/releases/download/v${V.sbt}/sbt-${V.sbt}.tgz",
            workDir
          )

          val installNodejs = bs.downloadFile(
            s"https://nodejs.org/dist/v${V.nodejs}/node-v${V.nodejs}-linux-x64.tar.xz",
            workDir,
            renameTo = "nodejs".some
          )

          val installNomad = bs.downloadFile(
            "https://releases.hashicorp.com/nomad/1.6.1/nomad_1.6.1_linux_amd64.zip",
            workDir / "nomad"
          )

          val installPacker = bs.cloneRepo(
            "https://github.com/wbthomason/packer.nvim",
            (workDir / "packer" / "start" / "packer.nvim").some,
            List("--depth", "1")
          )

          val writeFooConfig = bs.writeConfigFile(
            workDir / "foo.conf",
            s"""blah=blubb
             |foo=bar
             |baz=${V.nvim}""".stripMargin
          )

          val writeBarConfig = bs.writeConfigFile(
            workDir / "bar.conf",
            s"""blah=blubb
             |foo=bar
             |baz=${V.nvim}""".stripMargin
          )

          val echoHelloWorld = bs.executeBashScript("echo 'hello bash'")
          val testFishScript =
            bs.executeShellScript("echo 'hello fish'", "fish")

          (
            installNeovim,
            installScalaCli,
            installCoursier,
            installSbt,
            installPacker,
            installNodejs,
            writeFooConfig,
            writeBarConfig,
            echoHelloWorld,
            testFishScript,
            installNomad
          ).parTupled *> bs.logInfo(s"Finished writing to $workDir")

        }
    }

  }

  override def run: IO[Unit] = runF[IO]

}

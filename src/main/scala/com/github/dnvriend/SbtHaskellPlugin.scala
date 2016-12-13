/*
 * Copyright 2016 Dennis Vriend
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.dnvriend

import sbt.Keys._
import sbt._
import sbt.inc.Analysis

import scala.util.{ Failure, Try }
import scalaz._
import Scalaz._

object SbtHaskellPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    val HaskellCompile: Configuration = config("haskell-compile").describedAs("Haskell compile configuration")
    val HaskellTest: Configuration = config("haskell-test").describedAs("Haskell test configuration")

    val haskellExecutableName: SettingKey[String] = settingKey[String]("set the executable haskell file name; defaults to the project name and will be saved in 'target/haskell'")
    val haskellVerbosityLevel: SettingKey[Int] = settingKey[Int]("set verbosity level [1..3]; defaults to 1, can be changed by typing 'set haskellVerbosityLevel := 3'")
    val haskellCompilerCommand: SettingKey[String] = settingKey[String]("ghc")
    val haskellDownloadDir: SettingKey[File] = settingKey[File]("the haskell download dir; defaults to 'target/haskell/download'")
    val haskellOutputDir: SettingKey[File] = settingKey[File]("'-outputdir ⟨dir⟩': set output directory; defaults to 'target/haskell/output'")
    val haskellCompile: TaskKey[Analysis] = taskKey[Analysis]("compile haskell files")
    val haskellRun: TaskKey[Unit] = taskKey[Unit]("run the `Main.hs` haskell program")
    val haskellTest: TaskKey[Unit] = taskKey[Unit]("alias for ';clean;haskellCompile;haskellRun'")
    val haskellDownload: TaskKey[Disjunction[String, List[File]]] = taskKey[Disjunction[String, List[File]]]("(experimental) - download the haskellPackages")
    val cabalUpdate: TaskKey[Unit] = taskKey[Unit]("download the latest package list from 'hackage.haskell.org'")
    val cabalInstall: TaskKey[Unit] = taskKey[Unit]("download and install packages from Hackage defined in the settingKey 'cabalPackages'")
    val cabalPackages: SettingKey[Seq[String]] = settingKey[Seq[String]]("""list of Haskell packages to put on the library path, the format should be Seq("name-version") so for example Seq("adjunctions-4.3"); defaults to Seq.empty[String]""")
  }

  import autoImport._

  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    Keys.unmanagedSourceDirectories in HaskellCompile := Seq.empty[File],
    Keys.unmanagedSourceDirectories in HaskellTest := Seq.empty[File],
    Keys.unmanagedSourceDirectories in HaskellCompile += baseDirectory.value / "src" / "main" / "haskell",
    Keys.unmanagedSourceDirectories in HaskellTest += baseDirectory.value / "src" / "test" / "haskell",
    Keys.compile := Def.sequential(clean, haskellCompile, Keys.compile in Compile).value,
    Keys.test := Def.sequential(clean, haskellTest, Keys.test in Test).value,
    haskellCompile := HaskellCompiler.compile().value,
    haskellVerbosityLevel := 1,
    haskellExecutableName := name.value,
    haskellCompilerCommand := "ghc",
    Keys.target in HaskellCompile := Keys.target.value / "haskell",
    haskellOutputDir := (Keys.target in HaskellCompile).value / "output",
    haskellRun := Def.sequential(haskellCompile, HaskellRunner.run()).value,
    haskellTest := Def.sequential(HaskellTester.test()).value,
    cabalUpdate := "cabal update".!,
    cabalInstall := {
      val log = streams.value.log
      val packages = cabalPackages.value.toList.mkString(" ")
      val cmd = s"cabal install $packages"
      log.info(s"Executing command: '$cmd'")
      cmd.!
    },
    cabalPackages := Seq.empty[String],
    Keys.run := {
      (run in Compile).evaluated
      haskellRun.value
    },
    Keys.watchSources ++= (unmanagedSourceDirectories in HaskellCompile).value,
    Keys.watchSources ++= (unmanagedSourceDirectories in HaskellTest).value,
    Keys.pollInterval := 500,
    Keys.unmanagedSourceDirectories in Compile ++= (unmanagedSourceDirectories in HaskellCompile).value,
    Keys.unmanagedSourceDirectories in Test ++= (unmanagedSourceDirectories in HaskellTest).value
  )

  override def projectSettings: Seq[Setting[_]] =
    defaultSettings
}

object Util {
  final val HaskellFileExtension = ".hs"

  def createDir(dir: File)(implicit log: Logger): Try[Unit] = Try(IO.createDirectory(dir)).logError

  def createDirs(dirs: File*)(implicit log: Logger): Try[Unit] = Try(IO.createDirectories(dirs)).logError

  def info(msg: String)(implicit log: Logger): Try[Unit] = Try(log.info(msg)).logError

  def executeCommand(cmd: String)(implicit log: Logger): Try[Unit] = Try(Process(cmd).!!).map(log.info(_))

  implicit class TryOps[A](val that: Try[A]) extends AnyVal {
    def logError(implicit log: Logger) = that.recoverWith { case t: Throwable => log.error(s"Error: ${t.getMessage}"); Failure(t) }
  }

  val haskellSourceFilter = new FileFilter {
    override def accept(name: File): Boolean =
      name.isFile && name.absolutePath.endsWith(HaskellFileExtension)
  }

  val isDirectoryFilter = new FileFilter {
    override def accept(pathname: File): Boolean =
      pathname.isDirectory
  }

  def listFiles(dir: File)(implicit log: Logger): List[File] = {
    def loop(currentDir: File, acc: List[File]): List[File] = {
      val listOfDirs = IO.listFiles(currentDir, isDirectoryFilter).toList
      val listOfHaskellSources = IO.listFiles(currentDir, haskellSourceFilter).toList
      listOfHaskellSources ++ listOfDirs.flatMap(loop(_, List.empty[File]))
    }

    loop(dir, List.empty[File])
  }
}

object HaskellCompiler {

  import SbtHaskellPlugin.autoImport._
  import Util._

  def compile() = Def.task {
    implicit val log: Logger = streams.value.log
    val verbosityLevel: Int = haskellVerbosityLevel.value
    val compilerCommand: String = haskellCompilerCommand.value
    val targetDir: File = (target in HaskellCompile).value
    val outputDir: File = haskellOutputDir.value
    val sourceDirs: Seq[File] = (Keys.unmanagedSourceDirectories in HaskellCompile).value
    val executableName: String = haskellExecutableName.value
    val fullPathExecutable: String = s"""${targetDir.getAbsolutePath}/$executableName"""
    val listOfSourceFilesToCompile: List[String] = sourceDirs.toList.flatMap(listFiles).map(_.getAbsolutePath)
    val packages: String = cabalPackages.value.toList.map(name => s"-package $name").mkString(" ")
    val cmd: String = s"""$compilerCommand -v$verbosityLevel -outputdir $outputDir ${listOfSourceFilesToCompile.mkString(" ")} $packages -o $fullPathExecutable"""

    val result: Try[Unit] = for {
      _ <- createDirs(targetDir, outputDir)
      _ <- info(s"[Compile]: Executing command:\n'$cmd'")
      _ <- executeCommand(cmd)
    } yield ()

    result.recoverWith {
      case t: Throwable =>
        log.error(s"[Compile]: Error compiling Haskell source code: $t")
        scala.util.Failure(t)
    }

    Analysis.Empty
  }
}

object HaskellRunner {

  import SbtHaskellPlugin.autoImport._
  import Util._

  def run() = Def.task {
    implicit val log: Logger = streams.value.log
    val targetDir: File = (target in HaskellCompile).value
    val executableName: String = haskellExecutableName.value
    val fullPathExecutable: String = s"${targetDir.getAbsolutePath}/$executableName"
    val cmd: String = s"$fullPathExecutable"

    val result = for {
      _ <- info(s"[Run]: Executing command:\n'$cmd'")
      _ <- executeCommand(cmd)
    } yield ()

    result.recoverWith {
      case t: Throwable =>
        log.error(s"[Run]: Error running $t")
        scala.util.Failure(t)
    }

    ()
  }
}

object HaskellTester {
  import SbtHaskellPlugin.autoImport._
  import Util._

  def test() = Def.task {
    implicit val log: Logger = streams.value.log

    val verbosityLevel: Int = haskellVerbosityLevel.value
    val compilerCommand: String = haskellCompilerCommand.value
    val targetDir: File = (target in HaskellCompile).value
    val outputDir: File = haskellOutputDir.value
    val sourceDirs: Seq[File] = (Keys.unmanagedSourceDirectories in HaskellCompile).value
    val testDirs: Seq[File] = (Keys.unmanagedSourceDirectories in HaskellTest).value
    val executableName: String = haskellExecutableName.value
    val fullPathExecutable: String = s"""${targetDir.getAbsolutePath}/${executableName}Test"""
    val listOfSourceFilesToCompile: List[String] = sourceDirs.toList.flatMap(listFiles).filterNot(_.name.contains("Main.hs")).map(_.getAbsolutePath)
    val listOfTestFilesToCompile: List[String] = testDirs.toList.flatMap(listFiles).map(_.getAbsolutePath)
    val totalFilesToCompile: List[String] = listOfSourceFilesToCompile ++ listOfTestFilesToCompile
    val packages: String = cabalPackages.value.toList.map(name => s"-package $name").mkString(" ")

    val cmd: String = s"""$compilerCommand -v$verbosityLevel -outputdir $outputDir ${totalFilesToCompile.mkString(" ")} $packages -o $fullPathExecutable"""
    val runCmd: String = s"$fullPathExecutable"

    val result: Try[Unit] = for {
      _ <- createDirs(targetDir, outputDir)
      _ <- info(s"[Test]: Executing command:\n'$cmd'")
      _ <- executeCommand(cmd)
      _ <- info(s"[RunTest]: Executing command:\n'$runCmd'")
      _ <- executeCommand(runCmd)
    } yield ()

    result.recoverWith {
      case t: Throwable =>
        log.error(s"[Test]: Error compiling Haskell source code: $t")
        scala.util.Failure(t)
    }

    ()
  }
}

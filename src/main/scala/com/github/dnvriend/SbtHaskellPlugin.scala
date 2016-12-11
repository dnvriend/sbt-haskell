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

import scala.util.{ Failure, Try }
import scalaz._
import Scalaz._

object SbtHaskellPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    val haskellExecutableName: SettingKey[String] = settingKey[String]("set the executable haskell file name; defaults to the project name and will be saved in 'target/haskell'")
    val haskellVerbosityLevel: SettingKey[Int] = settingKey[Int]("set verbosity level [1..3]; defaults to 1, can be changed by typing 'set haskellVerbosityLevel := 3'")
    val haskellCompilerCommand: SettingKey[String] = settingKey[String]("ghc")
    val haskellSource: SettingKey[File] = settingKey[File]("the haskell source dir")
    val haskellTargetDir: SettingKey[File] = settingKey[File]("the haskell target dir; defaults to 'target/haskell'")
    val haskellDownloadDir: SettingKey[File] = settingKey[File]("the haskell download dir; defaults to 'target/haskell/download'")
    val haskellOutputDir: SettingKey[File] = settingKey[File]("'-outputdir ⟨dir⟩': set output directory; defaults to 'target/haskell/output'")
    val haskellCompile: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("compile haskell files")
    val haskellRun: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("run the `Main.hs` haskell program")
    val haskellTest: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("alias for ';clean;haskellCompile;haskellRun'")
    val haskellDownload: TaskKey[Disjunction[String, List[File]]] = taskKey[Disjunction[String, List[File]]]("(experimental) - download the haskellPackages")
    val cabalUpdate: TaskKey[Unit] = taskKey[Unit]("download the latest package list from 'hackage.haskell.org'")
    val cabalInstall: TaskKey[Unit] = taskKey[Unit]("download and install packages from Hackage defined in the settingKey 'cabalPackages'")
    val cabalPackages: SettingKey[Seq[String]] = settingKey[Seq[String]]("""list of Haskell packages to put on the library path, the format should be Seq("name-version") so for example Seq("adjunctions-4.3"); defaults to Seq.empty[String]""")
  }

  import autoImport._

  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    haskellVerbosityLevel := 1,
    haskellExecutableName := name.value,
    haskellCompilerCommand := "ghc",
    haskellSource := baseDirectory.value / "src" / "main" / "haskell",
    haskellTargetDir := target.value / "haskell",
    haskellOutputDir := haskellTargetDir.value / "output",
    haskellCompile := HaskellCompiler.compile().value,
    haskellRun := HaskellRunner.run().value,
    haskellTest := Def.sequential(clean, haskellCompile, haskellRun).value,
    cabalUpdate := "cabal update".!,
    cabalInstall := {
      val log = streams.value.log
      val packages = cabalPackages.value.toList.mkString(" ")
      val cmd = s"cabal install $packages"
      log.info(s"Executing command: '$cmd'")
      cmd.!
    },
    cabalPackages := Seq.empty[String],
    Keys.run := haskellRun.value,
    Keys.test := haskellTest.value,
    Keys.watchSources += haskellSource.value,
    Keys.pollInterval := 500,
    Keys.unmanagedSourceDirectories in Compile += haskellSource.value
  )

  override def projectSettings: Seq[Setting[_]] =
    defaultSettings
}

object Util {
  final val HaskellFileExtension = ".hs"

  def createDir(dir: File)(implicit log: Logger): Try[Unit] = Try(IO.createDirectory(dir)).logError

  def createDirs(dirs: File*)(implicit log: Logger): Try[Unit] = Try(IO.createDirectories(dirs)).logError

  def info(msg: String)(implicit log: Logger): Try[Unit] = Try(log.info(msg)).logError

  def executeCommand(cmd: String)(implicit log: Logger): Try[Int] = Try(Process(cmd).!(log)).logError

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
    val outputDir: File = haskellOutputDir.value
    val sourceDir: File = haskellSource.value
    val targetDir: File = haskellTargetDir.value
    val executableName: String = haskellExecutableName.value
    val fullPathExecutable: String = s"""${targetDir.getAbsolutePath}/$executableName"""
    val listOfSourceFilesToCompile: List[String] = listFiles(sourceDir).map(_.getAbsolutePath)
    val packages: String = cabalPackages.value.toList.map(name => s"-package $name").mkString(" ")
    val cmd: String = s"""$compilerCommand -v$verbosityLevel -outputdir $outputDir ${listOfSourceFilesToCompile.mkString(" ")} $packages -o $fullPathExecutable"""

    for {
      _ <- createDirs(targetDir, outputDir)
      _ <- info(s"Executing command:\n'$cmd'")
      _ <- executeCommand(cmd)
    } yield ()
  }
}

object HaskellRunner {

  import SbtHaskellPlugin.autoImport._
  import Util._

  def run() = Def.task {
    implicit val log: Logger = streams.value.log
    val targetDir: File = haskellTargetDir.value
    val executableName: String = haskellExecutableName.value
    val fullPathExecutable: String = s"${targetDir.getAbsolutePath}/$executableName"
    val cmd: String = s"$fullPathExecutable"

    for {
      _ <- info(s"Executing command:\n'$cmd'")
      _ <- executeCommand(cmd)
    } yield ()
  }
}

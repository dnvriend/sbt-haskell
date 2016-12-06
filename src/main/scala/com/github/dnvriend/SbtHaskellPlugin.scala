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

import sbt._
import sbt.Keys._

import scala.util.{ Failure, Try }

object SbtHaskellPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    val haskellExecutableName: SettingKey[String] = settingKey[String]("set the executable haskell file name; defaults to the project name and will be saved in 'target/haskell'")
    val haskellVerbosityLevel: SettingKey[Int] = settingKey[Int]("set verbosity level [1..3]; defaults to 1, can be changed by typing 'set haskellVerbosityLevel := 3'")
    val haskellCompilerCommand: SettingKey[String] = settingKey[String]("ghc")
    val haskellSource: SettingKey[File] = settingKey[File]("The haskell source dir")
    val haskellTargetDir: SettingKey[File] = settingKey[File]("The haskell target dir; defaults to 'target/haskell'")
    val haskellOutputDir: SettingKey[File] = settingKey[File]("'-outputdir ⟨dir⟩': set output directory; defaults to 'target/haskell/output'")
    val haskellCompile: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("compile haskell files")
    val haskellRun: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("Run haskell program")
    val haskellTest: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("alias for ';clean;haskellCompile;haskellRun'")
  }

  import autoImport._

  lazy val defaultSettings: Seq[Setting[_]] = Seq(
    haskellVerbosityLevel := 1,
    haskellExecutableName := name.value,
    haskellCompilerCommand := "ghc",
    haskellSource := baseDirectory.value / "src" / "main" / "haskell",
    watchSources += haskellSource.value,
    pollInterval := 500,
    haskellTargetDir := target.value / "haskell",
    haskellOutputDir := haskellTargetDir.value / "output",
    unmanagedSourceDirectories in Compile += haskellSource.value,
    haskellCompile := HaskellCompiler.compile().value,
    haskellRun := HaskellRunner.run().value,
    haskellTest := Def.sequential(clean, haskellCompile, haskellRun).value

  )

  override def projectSettings: Seq[Setting[_]] =
    defaultSettings
}

object Util {
  def createDir(dir: File)(implicit log: Logger): Try[Unit] = Try(IO.createDirectory(dir)).logError
  def createDirs(dirs: File*)(implicit log: Logger): Try[Unit] = Try(IO.createDirectories(dirs)).logError
  def info(msg: String)(implicit log: Logger): Try[Unit] = Try(log.info(msg)).logError
  def executeCommand(cmd: String)(implicit log: Logger): Try[Int] = Try(Process(cmd).!(log)).logError

  implicit class TryOps[A](val that: Try[A]) extends AnyVal {
    def logError(implicit log: Logger) = that.recoverWith { case t: Throwable => log.error(s"Error: ${t.getMessage}"); Failure(t) }
  }
}

object HaskellCompiler {

  import Util._
  import SbtHaskellPlugin.autoImport._

  def compile() = Def.task {
    implicit val log: Logger = streams.value.log
    val verbosityLevel: Int = haskellVerbosityLevel.value
    val compilerCommand: String = haskellCompilerCommand.value
    val outputDir: File = haskellOutputDir.value
    val sourceDir: File = haskellSource.value
    val targetDir: File = haskellTargetDir.value
    val executableName: String = haskellExecutableName.value
    val fullPathExecutable: String = s"""${targetDir.getAbsolutePath}/$executableName"""
    val listOfSourceFilesToCompile = IO.listFiles(sourceDir).map(file => file.getAbsolutePath)
    val cmd: String = s"""$compilerCommand -v$verbosityLevel -outputdir $outputDir ${listOfSourceFilesToCompile.mkString(" ")} -o $fullPathExecutable"""

    for {
      _ <- createDirs(targetDir, outputDir)
      _ <- info(s"Executing command:\n'$cmd'")
      _ <- executeCommand(cmd)
    } yield ()
  }
}

object HaskellRunner {

  import Util._
  import SbtHaskellPlugin.autoImport._

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

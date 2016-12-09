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

import java.io.InputStream

import com.github.dnvriend.FastParseImplicits._
import fastparse.all._
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.archivers.tar.{ TarArchiveEntry, TarArchiveInputStream }
import org.apache.commons.compress.utils.IOUtils
import sbt.Keys._
import sbt._

import scala.util.{ Failure, Try }
import scalaz.Scalaz._
import scalaz._

object SbtHaskellPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    val haskellExecutableName: SettingKey[String] = settingKey[String]("set the executable haskell file name; defaults to the project name and will be saved in 'target/haskell'")
    val haskellVerbosityLevel: SettingKey[Int] = settingKey[Int]("set verbosity level [1..3]; defaults to 1, can be changed by typing 'set haskellVerbosityLevel := 3'")
    val haskellCompilerCommand: SettingKey[String] = settingKey[String]("ghc")
    val haskellSource: SettingKey[File] = settingKey[File]("The haskell source dir")
    val haskellTargetDir: SettingKey[File] = settingKey[File]("The haskell target dir; defaults to 'target/haskell'")
    val haskellDownloadDir: SettingKey[File] = settingKey[File]("The haskell download dir; defaults to 'target/haskell/download'")
    val haskellOutputDir: SettingKey[File] = settingKey[File]("'-outputdir ⟨dir⟩': set output directory; defaults to 'target/haskell/output'")
    val haskellCompile: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("compile haskell files")
    val haskellRun: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("Run haskell program")
    val haskellTest: TaskKey[Try[Unit]] = taskKey[Try[Unit]]("alias for ';clean;haskellCompile;haskellRun'")
    val haskellPackages: SettingKey[Seq[String]] = settingKey[Seq[String]]("""List of Haskell packages to download from Hackage and extract to your haskellSource directory, the format should be Seq("name:version") so for example Seq("adjunctions:4.3"); defaults to Seq.empty[String]""")
    val haskellDownload: TaskKey[Disjunction[String, List[File]]] = taskKey[Disjunction[String, List[File]]]("Download the haskellPackages")
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
    haskellPackages := Seq.empty[String],
    haskellDownloadDir := haskellTargetDir.value / "download",
    haskellDownload := Hackage.validateAndDownload().value,
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

object Hackage {
  def validateHaskellPackage(input: String): ValidationNel[String, Hackage] = {
    val hackagePackage = CharIn('a' to 'z', 'A' to 'Z', "-")
    val hackageVersion = CharIn('0' to '9', ".")
    val hackageParser: Parser[Hackage] = P(hackagePackage.rep.! ~ ":" ~ hackageVersion.rep.! ~ End).map {
      case (packageName, version) => Hackage(packageName, version)
    }
    hackageParser.parse(input).validation.keepType
  }

  def packageUrl(packageName: String, version: String)(implicit log: Logger): Disjunction[String, URL] = {
    val urlAsString = s"http://hackage.haskell.org/package/$packageName-$version/$packageName-$version.tar.gz"
    Disjunction.fromTryCatchNonFatal {
      url(urlAsString)
    }.leftMap(ex => s"Could not create URL from for URL: '$urlAsString', error: $ex")
  }

  def descriptionUrl(packageName: String, version: String)(implicit log: Logger): Disjunction[String, URL] = {
    val urlAsString = s"http://hackage.haskell.org/package/$packageName-$version/$packageName.cabal"
    Disjunction.fromTryCatchNonFatal {
      url(urlAsString)
    }.leftMap(ex => s"Could not create descriptionUrl for URL: '$urlAsString', error: $ex")
  }

  def downloadDescription(descriptionUrl: URL, toDir: File): Disjunction[String, File] = Disjunction.fromTryCatchNonFatal {
    IO.download(descriptionUrl, toDir)
    toDir
  }.leftMap(ex => s"Could not download description: $ex")

  def download(url: URL, toFile: File)(implicit log: Logger): Disjunction[String, File] = Disjunction.fromTryCatchNonFatal {
    IO.download(url, toFile)
    log.info(s"Downloaded from url: '$url' to file: '${toFile.absolutePath}'")
    toFile
  }.leftMap(ex => s"Could not download library from url $url to file ${toFile.absolutePath}; error: ${ex.getMessage}")

  def unzip(downloaded: File, toFile: File): Disjunction[String, File] = Disjunction.fromTryCatchNonFatal {
    IO.gunzip(downloaded, toFile)
    toFile
  }.leftMap(ex => s"Error unzipping '${downloaded.absolutePath}': ${ex.getMessage}")

  object TarInputStreamIterator {
    def apply(is: TarArchiveInputStream): TarInputStreamIterator =
      new TarInputStreamIterator(is)

    def tarArchiveInputStream(is: InputStream): TarArchiveInputStream =
      new ArchiveStreamFactory().createArchiveInputStream("tar", is).asInstanceOf[TarArchiveInputStream]
  }

  class TarInputStreamIterator(is: TarArchiveInputStream) extends Iterator[TarArchiveEntry] {
    var buffer: Option[TarArchiveEntry] = none
    def none = Option.empty[TarArchiveEntry]
    def some(entry: TarArchiveEntry) = Option(entry)
    override def hasNext: Boolean = {
      buffer = Try(Option(is.getNextTarEntry)).toOption.flatten
      buffer.isDefined
    }
    override def next(): TarArchiveEntry = buffer.get
  }

  def blackListed(entry: TarArchiveEntry, hackage: Hackage): Boolean = {
    val hackageFileName = s"${hackage.packageName}-${hackage.version}/"
    Option(entry).exists(_.getName.toLowerCase.endsWith(".cabal")) ||
      Option(entry).exists(_.getName.toLowerCase.endsWith(".md")) ||
      Option(entry).exists(_.getName.toLowerCase.contains("license")) ||
      Option(entry).exists(_.getName.toLowerCase.contains("setup.hs")) ||
      Option(entry).exists(entry => entry.getName == hackageFileName && entry.isDirectory) ||
      Option(entry).exists(entry => entry.getName.contains("tests") && entry.isDirectory)
  }

  def extract(tar: File, toDir: File, hackage: Hackage, sourceDirs: List[String])(implicit log: Logger): Disjunction[String, List[File]] = Disjunction.fromTryCatchNonFatal {
    val hackageFileName = if (sourceDirs.isEmpty) {
      s"${hackage.packageName}-${hackage.version}"
    } else {
      s"${hackage.packageName}-${hackage.version}/${sourceDirs.head}"
    }

    def entryName(entry: TarArchiveEntry) = {
      val oldName = entry.getName
      oldName.substring(hackageFileName.length)
    }
    def directoryFromEntry(entry: TarArchiveEntry): File = file(s"${toDir.absolutePath}/${entryName(entry)}")
    def fileFromEntry(entry: TarArchiveEntry): File = file(s"${toDir.absolutePath}/${entryName(entry)}")
    var processedFiles: List[File] = List.empty[File]
    Using.fileInputStream(tar) { is =>
      val tais = TarInputStreamIterator.tarArchiveInputStream(is)
      TarInputStreamIterator(tais)
        .foreach {
          case entry if blackListed(entry, hackage) =>
            log.info(s"Skipping entry: ${entry.getName}")
          case entry if entry.isDirectory =>
            log.info(s"Creating directory: ${directoryFromEntry(entry).absolutePath}")
            IO.createDirectory(directoryFromEntry(entry))
          case entry =>
            val fileToCreate = fileFromEntry(entry)
            log.info(s"Creating file: ${fileToCreate.absolutePath}")
            Using.fileOutputStream()(fileToCreate) { out =>
              IOUtils.copy(tais, out)
            }
            processedFiles = processedFiles :+ fileToCreate
        }
    }
    processedFiles
  }.leftMap { ex =>
    ex.printStackTrace()
    s"Error extracting ${tar.absolutePath} to dir: ${toDir.absolutePath}"
  }

  def determineSourceDirs(descriptionFile: File): Disjunction[String, List[String]] = Disjunction.fromTryCatchNonFatal {
    IO.read(descriptionFile)
      .split("\n")
      .map(_.trim)
      .filter(line => line.startsWith("hs-source-dirs") || line.startsWith("hs-source-dir") || line.startsWith("Hs-Source-Dirs"))
      .flatMap(_.split(":").drop(1).headOption.map(_.trim).filter(_.nonEmpty))
      .toList
  }.leftMap(ex => s"$ex")

  def downloadHackage(hackage: Hackage, sourceDir: File, downloadDir: File)(implicit log: Logger): Disjunction[String, List[File]] = {
    val tgzName = new File(s"${downloadDir.absolutePath}/${hackage.packageName}-${hackage.version}.tgz")
    val tarName = new File(s"${downloadDir.absolutePath}/${hackage.packageName}-${hackage.version}.tar")
    val descriptionFile = new File(s"${downloadDir.absolutePath}/${hackage.packageName}.cabal")
    for {
      packageUrl <- packageUrl(hackage.packageName, hackage.version)
      descriptionUrl <- descriptionUrl(hackage.packageName, hackage.version)
      _ <- Disjunction.fromTryCatchNonFatal(IO.createDirectory(downloadDir)).leftMap(ex => s"$ex")
      description <- downloadDescription(descriptionUrl, descriptionFile)
      sourceDirs <- determineSourceDirs(description)
      downloaded <- download(packageUrl, tgzName)
      upzipped <- unzip(downloaded, tarName)
      processedFiles <- extract(tarName, sourceDir, hackage, sourceDirs)
    } yield processedFiles
  }

  def downloadListOfHackageResources(listOfHackageResources: List[Hackage], sourceDir: File, downloadDir: File)(implicit log: Logger): Disjunction[String, List[File]] = {
    def loop(jobs: List[Hackage], acc: List[Disjunction[String, List[File]]]): List[Disjunction[String, List[File]]] =
      if (jobs.isEmpty) acc else loop(jobs.tail, downloadHackage(jobs.head, sourceDir, downloadDir) +: acc)

    loop(listOfHackageResources, List.empty[Disjunction[String, List[File]]])
      .sequenceU
      .rightMap(_.flatten)
  }

  def validateAndDownload() = Def.task {
    import SbtHaskellPlugin.autoImport._
    implicit val log: Logger = streams.value.log
    val sourceDir: File = haskellSource.value
    val downloadDir: File = haskellDownloadDir.value
    val listOfHaskellPackages: Seq[String] = haskellPackages.value
    val listOfValidatedHackageResources: ValidationNel[String, List[Hackage]] =
      listOfHaskellPackages.toList.traverseU(Hackage.validateHaskellPackage)

    val result: Disjunction[String, List[File]] = for {
      listOfHackageResources <- listOfValidatedHackageResources.disjunction.leftMap(_.toList.mkString(","))
      downloadedFiles <- downloadListOfHackageResources(listOfHackageResources, sourceDir, downloadDir)
    } yield downloadedFiles

    result match {
      case DLeft(errors) =>
        log.info(s"Errors while downloading Haskell Packages: $errors")
      case DRight(_) if listOfHaskellPackages.isEmpty =>
        log.info("No Haskell packages configured to download")
      case DRight(files) if files.isEmpty =>
        log.info("No Haskell packages downloaded")
      case DRight(files) =>
        val xs = files.map(_.absolutePath)
        log.info(s"Successfully downloaded Haskell packages containing the following files: $xs")
    }

    result
  }
}

final case class Hackage(packageName: String, version: String)

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
    val cmd: String = s"""$compilerCommand -v$verbosityLevel -outputdir $outputDir ${listOfSourceFilesToCompile.mkString(" ")} -o $fullPathExecutable"""

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

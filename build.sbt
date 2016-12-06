name := "sbt-haskell"

organization := "com.github.dnvriend"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.6"

sbtPlugin := true

licenses +=("Apache-2.0", url("http://opensource.org/licenses/apache2.0.php"))

// enable scala code formatting //
import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform

// Scalariform settings
SbtScalariform.autoImport.scalariformPreferences := SbtScalariform.autoImport.scalariformPreferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 100)
  .setPreference(DoubleIndentClassDeclaration, true)

// enable updating file headers //
import de.heikoseeberger.sbtheader.license.Apache2_0

headers := Map(
  "scala" -> Apache2_0("2016", "Dennis Vriend"),
  "conf" -> Apache2_0("2016", "Dennis Vriend", "#")
)

// enable publishing to jcenter
homepage := Some(url("https://github.com/dnvriend/sbt-haskell"))

pomIncludeRepository := (_ => false)

pomExtra := <scm>
  <url>https://github.com/dnvriend/sbt-haskell</url>
  <connection>scm:git@github.com:dnvriend/sbt-haskell.git</connection>
</scm>
  <developers>
    <developer>
      <id>dnvriend</id>
      <name>Dennis Vriend</name>
      <url>https://github.com/dnvriend</url>
    </developer>
  </developers>

publishMavenStyle := true

bintrayPackageLabels := Seq("haskell", "sbt", "compile", "test")

bintrayPackageAttributes ~=
  (_ ++ Map(
    "website_url" -> Seq(bintry.Attr.String("https://github.com/dnvriend/sbt-haskell")),
    "github_repo" -> Seq(bintry.Attr.String("https://github.com/dnvriend/sbt-haskell.git")),
    "issue_tracker_url" -> Seq(bintry.Attr.String("https://github.com/dnvriend/sbt-haskell/issues/"))
  )
    )

enablePlugins(AutomateHeaderPlugin, SbtScalariform)

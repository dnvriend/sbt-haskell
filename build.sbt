name := "sbt-haskell"

organization := "com.github.dnvriend"

version := "0.0.2"

scalaVersion := "2.10.6"

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

publishMavenStyle := false

sbtPlugin := true

bintrayRepository := "sbt-plugins"

bintrayReleaseOnPublish := false

bintrayPackageLabels := Seq("haskell", "sbt", "compile", "test")

bintrayPackageAttributes ~=
  (_ ++ Map(
    "website_url" -> Seq(bintry.Attr.String("https://github.com/dnvriend/sbt-haskell")),
    "github_repo" -> Seq(bintry.Attr.String("https://github.com/dnvriend/sbt-haskell.git")),
    "issue_tracker_url" -> Seq(bintry.Attr.String("https://github.com/dnvriend/sbt-haskell/issues/"))
  )
)

enablePlugins(AutomateHeaderPlugin, SbtScalariform, BintrayPlugin)

import sbt.{CrossVersion, ThisBuild}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

import scala.sys.process._

val scalaVersion_ = "2.13.16"

lazy val copyrightSettings = Seq(
  startYear        := Some(2018),
  organizationName := "The Diesel Authors",
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
)

addCommandAlias("lint", "fmtCheck;fixCheck;headerCheckAll")
addCommandAlias("build", "compile;fastOptJS")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("lintFix", "headerCreateAll;scalafixAll;fmt")

inThisBuild(
  List(
    organization  := "com.ibm.cloud.diesel",
    scalaVersion  := scalaVersion_,
    versionScheme := Some("semver-spec")
  )
)

lazy val root: Project = project
  .in(file("."))
  .aggregate(dieselJVM, dieselJS, jsFacade)
  .settings(copyrightSettings)
  .settings(
    name         := "diesel-json-schema-root",
    scalaVersion := scalaVersion_
    // publish / skip := true
  )

ThisBuild / evictionErrorLevel := Level.Warn

lazy val diesel = crossProject(JSPlatform, JVMPlatform)
  .enablePlugins(I18nPlugin)
  .settings(copyrightSettings)
  .settings(
    name          := "diesel-json-schema",
    scalaVersion  := scalaVersion_,
    i18nDir       := file("./diesel/i18n"),
    i18nClassName := "diesel.json.i18n.I18nFiles"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.ibm.cloud.diesel" %%% "diesel-i18n"     % Dependencies.dieselI18nVersion,
      "com.ibm.cloud.diesel" %%% "diesel-core"     % Dependencies.dieselVersion,
      "io.github.cquiroz"    %%% "scala-java-time" % "2.6.0",
      "org.typelevel"        %%% "jawn-parser"     % "1.6.0"
    )
  )
  .settings(
    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-feature",
      "-Xfatal-warnings",
      "-language:existentials",
      "-Wunused:imports"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.0.2" % Test
    )
  )
  .settings(
    Test / fork        := false,
    Test / logBuffered := false,
    // see https://github.com/scalameta/munit/blob/main/junit-interface/src/main/java/munit/internal/junitinterface/JUnitRunner.java
    Test / testOptions += Tests.Argument("+l", "--summary=1")
  )
  .settings(
    ThisBuild / semanticdbEnabled := true,
    ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
  )

lazy val jsFacade = project
  .in(file("js-facade"))
  .dependsOn(dieselJS)
  .settings(copyrightSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.0.2" % Test
    ),
    // see https://github.com/scalameta/munit/blob/main/junit-interface/src/main/java/munit/internal/junitinterface/JUnitRunner.java
    Test / testOptions += Tests.Argument("+l", "--summary=1")
  )

import sbt.Keys.streams
lazy val copyrightSettingsTS = Seq(
  headerMappings := headerMappings.value + (HeaderFileType("ts") -> HeaderCommentStyle.cStyleBlockComment),
  Compile / headerSources ++= {
    fileTreeView.value.list(
      Glob(baseDirectory.value.toPath) / "{src,test}" / ** / "*.{ts*,*css}"
    ).map(
      _._1.toFile
    )
  }
)

lazy val tsFacade = project
  .in(file("ts-facade"))
  .settings(copyrightSettingsTS)
  .settings(
    cleanFiles += baseDirectory.value / "dist"
  )

lazy val dieselJS  = diesel.js
lazy val dieselJVM = diesel.jvm

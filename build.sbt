import sbt.{CrossVersion, ThisBuild}
import sbtcrossproject.CrossPlugin.autoImport.crossProject

import scala.sys.process._

val scalaVersion_ = "2.13.10"

lazy val copyrightSettings = Seq(
  startYear := Some(2018),
  organizationName := "The Diesel Authors",
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
)

addCommandAlias("lint", "fmtCheck;fixCheck;headerCheckAll")
addCommandAlias("build", "compile;fastOptJS;yarnAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("lintFix", "headerCreateAll;scalafixAll;fmt")
addCommandAlias("yarnAll", "yarnInstall;yarnBuild")

inThisBuild(
  List(
    organization := "com.ibm.cloud.diesel",
    scalaVersion := scalaVersion_,
    versionScheme := Some("semver-spec")
  )
)

lazy val root: Project = project
  .in(file("."))
  .aggregate(dieselJVM, dieselJS, jsFacade)
  .settings(copyrightSettings)
  .settings(
    name := "diesel-json-schema-root",
    scalaVersion := scalaVersion_
    // publish / skip := true
  )
  .settings(
    yarnInstall := "yarn install".!
  )

ThisBuild / evictionErrorLevel := Level.Warn

lazy val diesel = crossProject(JSPlatform, JVMPlatform)
  .enablePlugins(I18nPlugin)
  .settings(copyrightSettings)
  .settings(
    name := "diesel-json-schema",
    scalaVersion := scalaVersion_,
    i18nDir := file("./diesel/i18n"),
    i18nClassName := "diesel.json.i18n.I18nFiles"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.ibm.cloud.diesel" %%% "diesel-i18n"     % Dependencies.dieselI18nVersion,
      "com.ibm.cloud.diesel" %%% "diesel-core"     % Dependencies.dieselVersion,
      "io.github.cquiroz"    %%% "scala-java-time" % "2.3.0"
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
      "org.scalameta" %%% "munit" % "0.7.29" % Test
    )
  )
  .settings(
    Test / fork := false,
    Test / logBuffered := false,
    // see https://github.com/scalameta/munit/blob/main/junit-interface/src/main/java/munit/internal/junitinterface/JUnitRunner.java
    Test / testOptions += Tests.Argument("+l", "--summary=1")
  )
  .settings(
    addCompilerPlugin(scalafixSemanticdb),
    ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value),
    ThisBuild / semanticdbEnabled := true,
    ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
  )

lazy val jsFacade = project
  .in(file("js-facade"))
  .dependsOn(dieselJS)
  .settings(copyrightSettingsTS)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "0.7.29" % Test
    ),
    // see https://github.com/scalameta/munit/blob/main/junit-interface/src/main/java/munit/internal/junitinterface/JUnitRunner.java
    Test / testOptions += Tests.Argument("+l", "--summary=1")
  )
  .settings(
    yarnPack := {
      scalajsbundler.Yarn.run("pack")(baseDirectory.value, streams.value.log)
    },
    yarnPublish := {
      scalajsbundler.Yarn.run("publish", "--no-git-tag-version")(baseDirectory.value, streams.value.log)
    }
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

lazy val yarnInstall = taskKey[Unit]("yarn install")
lazy val yarnBuild   = taskKey[Unit]("yarn build")
lazy val yarnLint    = taskKey[Unit]("yarn lint")
lazy val yarnLintFix = taskKey[Unit]("yarn lint:fix")
lazy val yarnPublish = taskKey[Unit]("yarn publish")
lazy val yarnPack    = taskKey[Unit]("yarn pack")

// lazy val tsFacade = project
//   .in(file("ts-facade"))
//   .settings(copyrightSettingsTS)
//   .settings(
//       cleanFiles += baseDirectory.value / "dist"
//     )
//   .settings(
//     yarnBuild := {
//       scalajsbundler.Yarn.run("build")(baseDirectory.value, streams.value.log)
//     },
//     yarnLint := {
//       scalajsbundler.Yarn.run("lint")(baseDirectory.value, streams.value.log)
//     },
//     yarnLintFix := {
//       scalajsbundler.Yarn.run("lint:fix")(baseDirectory.value, streams.value.log)
//     },
//     yarnPack := {
//       val dep = yarnBuild.value
//       scalajsbundler.Yarn.run("pack")(baseDirectory.value, streams.value.log)
//     },
//     yarnPublish := {
//       val dep = yarnBuild.value
//       scalajsbundler.Yarn.run("publish", "--no-git-tag-version")(baseDirectory.value, streams.value.log)
//     }
//   )

lazy val dieselJS  = diesel.js
lazy val dieselJVM = diesel.jvm

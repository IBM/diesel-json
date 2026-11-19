enablePlugins(ScalaJSPlugin)

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

name := "diesel-json-schema-facade"

//// TODO?
scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-Wunused:imports",
  "-rewrite",
  "-source:3.8-migration"
)

Compile / fastOptJS := Def.uncached {
  val file = (Compile / fastOptJS).value
  IO.copyFile(file.data, baseDirectory.value / "dist" / "json-schema-facade.js")
  file
}

cleanFiles += baseDirectory.value / "dist"

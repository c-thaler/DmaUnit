name := "DmaUnit"
version := "1.0"
scalaVersion := "2.12.9"
val spinalVersion = "1.4.0"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.12" % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-lib_2.12" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.12" % spinalVersion),
  "org.scala-lang.plugins" % "scala-continuations-library_2.12" % "1.0.3",
  compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.12.2" % "1.0.3"),
)

fork := true
EclipseKeys.withSource := true

autoCompilerPlugins := true
//scalacOptions += "-P:continuations:enable"

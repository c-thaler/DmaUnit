name := "DmaUnit"
version := "1.0"
scalaVersion := "2.11.12"
val spinalVersion = "1.4.1"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion),
  //"org.scala-lang.plugins" % "scala-continuations-library_2.11" % "1.0.3",
  //compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.3"),
)

lazy val root = (project in file("."))

fork := true
EclipseKeys.withSource := true

autoCompilerPlugins := true
//scalacOptions += "-P:continuations:enable"

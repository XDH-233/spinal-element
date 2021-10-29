name    := "spinal-element"
version := "0.1"
//addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.1.2")
scalaVersion := "2.11.12"
val spinalVersion = "1.5.0"

libraryDependencies ++= Seq(
    "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
    "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
    compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion)
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9"

fork := true

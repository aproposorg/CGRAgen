// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "1.0.0"
ThisBuild / organization     := "Tampere University"

val chiselVersion = "3.5.4"

lazy val approx = RootProject(file("./approx"))
lazy val chiselverify = RootProject(file("./chiselverify"))

lazy val root = (project in file("."))
  .settings(
    name := "cgragen",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.5.4"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
      "-P:chiselplugin:genBundleElements",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  )
  .dependsOn(
    approx,
    chiselverify
  )

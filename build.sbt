lazy val commonSettings = Seq(
  organization := "nz.rd.frolic",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.3",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

lazy val root = (project in file("."))
    .aggregate(conk, `frolic-core`, `frolic-undertow`)

lazy val conk = project.settings(
  commonSettings
)

lazy val `conk-opentracing` = project.dependsOn(conk).settings(
  commonSettings,
  libraryDependencies ++= Seq(
    // OpenTracing
    "io.opentracing" % "opentracing-api" % "0.30.0",
    "io.opentracing" % "opentracing-util" % "0.30.0",
    "io.opentracing" % "opentracing-noop" % "0.30.0",
    "io.opentracing" % "opentracing-mock" % "0.30.0"
  )
)

lazy val `frolic-core` = project.dependsOn(conk).settings(
  commonSettings
)

lazy val `frolic-undertow` = project.dependsOn(`frolic-core`, conk, `conk-opentracing`).settings(
  commonSettings,
  libraryDependencies += "io.undertow" % "undertow-core" % "1.4.12.Final"
)

lazy val `frolic-dev` = project.dependsOn(conk, `conk-opentracing`, `frolic-undertow`).settings(
  commonSettings,
  libraryDependencies += "com.googlecode.lanterna" % "lanterna" % "3.0.0"
)

lazy val example = project.dependsOn(`frolic-dev`).settings(
  commonSettings,
  libraryDependencies ++= Seq(
    "io.opentracing.brave" % "brave-opentracing" % "0.21.0",
    "io.zipkin.reporter" % "zipkin-sender-okhttp3" % "1.0.0"
  )
)
name := "frolic"
organization := "nz.rd.frolic"
version := "0.1.0"
scalaVersion := "2.12.3"
libraryDependencies ++= Seq(
  // HTTP backend
  "io.undertow" % "undertow-core" % "1.4.12.Final",

  // Terminal display
  "com.googlecode.lanterna" % "lanterna" % "3.0.0",

  // OpenTracing
  "io.opentracing" % "opentracing-api" % "0.30.0",
  "io.opentracing" % "opentracing-util" % "0.30.0",
  "io.opentracing" % "opentracing-mock" % "0.30.0",
  // Zipkin
  "io.opentracing.brave" % "brave-opentracing" % "0.21.0",
  "io.zipkin.reporter" % "zipkin-sender-okhttp3" % "1.0.0",

  // Testing
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

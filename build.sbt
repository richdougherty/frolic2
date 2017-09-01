name := "frolic"
organization := "nz.rd.frolic"
version := "0.1.0"
scalaVersion := "2.12.2"
libraryDependencies ++= Seq(
  "io.undertow" % "undertow-core" % "1.4.12.Final",
  "io.opentracing" % "opentracing-api" % "0.30.0",
  "io.opentracing" % "opentracing-util" % "0.30.0",
  "io.opentracing" % "opentracing-mock" % "0.30.0",

//  "com.uber.jaeger" % "jaeger-core" % "0.18.0",

//  "io.zipkin.brave" % "brave" % "4.2.0",
  "io.opentracing.brave" % "brave-opentracing" % "0.21.0",
//  "io.zipkin.reporter" % "zipkin-reporter" % "0.7.1",
  "io.zipkin.reporter" % "zipkin-sender-okhttp3" % "1.0.0",

  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

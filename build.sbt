name := "frolic"
organization := "nz.rd.frolic"
version := "0.1.0"
scalaVersion := "2.11.8"
libraryDependencies ++= Seq(
  "io.netty" % "netty-handler" % "4.1.5.Final",
  "io.netty" % "netty-codec-http" % "4.1.5.Final",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

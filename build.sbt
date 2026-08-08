name := "patch-based-ga"
version := "0.1"
scalaVersion := "3.8.4"
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "3.1.1", // for parsing the configs
  "org.scalatest" %% "scalatest" % "3.2.20" % Test, // for unit testing
)

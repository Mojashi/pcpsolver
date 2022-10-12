ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "pcpsolver"
  )
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test
libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.21.7"
libraryDependencies += "com.google.guava" % "guava"% "30.1.1-jre"

libraryDependencies += "org.iq80.leveldb" % "leveldb" % "0.12"
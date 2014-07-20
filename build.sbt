scalaVersion := "2.11.2"

name := "foetus"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _ % "provided")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

scalaVersion := "2.11.0"

name := "foetus"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test"

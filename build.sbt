scalaVersion := "2.10.3"

name := "foetus"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

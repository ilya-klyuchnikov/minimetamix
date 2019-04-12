scalaVersion := "2.13.0-RC1"

name := "foetus"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8-RC2" % Test

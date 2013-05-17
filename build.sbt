scalaVersion := "2.10.1"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies  += ("org.scalatest" %% "scalatest" % "1.9.1" % "test")

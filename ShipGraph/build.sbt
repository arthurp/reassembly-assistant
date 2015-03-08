lazy val root = (project in file(".")).
  settings(
    name := "ShipGraph",
    version := "0.0.1",
    scalaVersion := "2.11.5"
  )
  
transitiveClassifiers := Seq("sources")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"
libraryDependencies += "org.specs2" %% "specs2-core" % "3.0" % "test"
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.0" % "test"
libraryDependencies += "org.specs2" %% "specs2-junit" % "3.0" % "test"
libraryDependencies += "junit" % "junit" % "4.11" % "test"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")
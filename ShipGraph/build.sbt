lazy val root = (project in file(".")).
  settings(
    name := "ShipGraph",
    version := "0.0.1",
    scalaVersion := "2.11.1"
  )
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"
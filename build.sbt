scalaVersion := "2.12.3"

name := "scala-musings"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-language:higherKinds"
)
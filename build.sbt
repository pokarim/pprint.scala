name := "pprint.scala"

version := "0.0.1"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2-core" % "3.7" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

scalaVersion := "2.11.7"

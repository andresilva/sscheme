name := "sscheme"

version := "0.1"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "snapshots"           at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"            at "http://oss.sonatype.org/content/repositories/releases",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.4")

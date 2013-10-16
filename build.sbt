import ReleaseKeys._
import sbtrelease.{Version,versionFormatError}

organization := "com.benkolera"

name := "slick-extra"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-feature","-deprecation","-Xfatal-warnings")

libraryDependencies ++= Seq(
  "com.typesafe.slick"     %% "slick"       % "1.0.1",
  "com.github.nscala-time" %% "nscala-time" % "0.6.0",
  "org.postgresql"          % "postgresql"  % "9.2-1003-jdbc4",
  "org.scalaz"             %% "scalaz-core" % "7.0.4",
  "org.specs2"             %% "specs2"      % "2.2.2" % "test"
)

resolvers ++= Seq()

releaseSettings

nextVersion := { ver =>
  Version(ver).map(_.bumpBugfix.asSnapshot.string).getOrElse(versionFormatError)
}

//Make this publish to oss.sonatype.com later
publishTo <<= version { (v: String) =>
  val nexus = "http://nexus.benkolera.com/nexus/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots/")
  else
    Some("releases"  at nexus + "content/repositories/releases/")
}

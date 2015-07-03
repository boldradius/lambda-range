import sbtrelease.ReleasePlugin.ReleaseKeys._

import sbtrelease.ReleasePlugin._

name := """brome"""

useGlobalVersion := false

organization := "com.boldradius"

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

javacOptions ++= Seq("-source", "1.6", "-target", "1.6")

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.10.4", "2.11.7")

publishMavenStyle := true

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

bintraySettings

releaseSettings

bintray.Keys.bintrayOrganization in bintray.Keys.bintray := Some("boldradiussolutions")

com.typesafe.sbt.SbtGit.versionWithGit


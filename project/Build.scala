package expression

import sbt._
import Keys._

object ScaldingBuild extends Build {

  def scalaBinaryVersion(scalaVersion: String) = scalaVersion match {
    case version if version startsWith "2.10" => "2.10"
    case version if version startsWith "2.11" => "2.11"
    case _ => sys.error("unknown error")
  }
  def isScala210x(scalaVersion: String) = scalaBinaryVersion(scalaVersion) == "2.10"

  val scalaCheckVersion = "1.12.2"
  val scalaTestVersion = "2.2.4"
  val slf4jVersion = "1.6.6"

  val sharedSettings = Project.defaultSettings ++ Seq(
    organization := "com.twitter",

    scalaVersion := "2.10.4",

    crossScalaVersions := Seq("2.10.4", "2.11.5"),

    libraryDependencies ++= Seq(
      "org.mockito" % "mockito-all" % "1.8.5" % "test",
      "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
      "org.slf4j" % "slf4j-log4j12" % slf4jVersion % "test"
    ),

    resolvers ++= Seq(
      "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
      "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "releases" at "https://oss.sonatype.org/content/repositories/releases"
    ),

    fork in Test := true,

    javaOptions in Test ++= Seq("-Xmx2048m", "-XX:ReservedCodeCacheSize=384m", "-XX:MaxPermSize=384m"),

    concurrentRestrictions in Global := Seq(
      Tags.limitAll(1)
    ),

    parallelExecution in Test := false,

    scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-language:higherKinds", "-language:existentials"),

    scalacOptions <++= (scalaVersion) map { sv =>
        if (isScala210x(sv))
          Seq("-Xdivergence211")
        else
          Seq()
    },

    // Enables full stack traces in scalatest
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),

    // Publishing options:

    publishMavenStyle := true,

    publishArtifact in Test := false,

    pomIncludeRepository := {
      x => false
    },

    publishTo <<= version { v =>
      Some(
        if (v.trim.endsWith("SNAPSHOT"))
          Opts.resolver.sonatypeSnapshots
        else
          Opts.resolver.sonatypeStaging
          //"twttr" at "http://artifactory.local.twitter.com/libs-releases-local"
      )
    },

    pomExtra := (
      <url>https://github.com/johnynek/expression</url>
        <licenses>
          <license>
            <name>Apache 2</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
            <distribution>repo</distribution>
            <comments>A business-friendly OSS license</comments>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:johnynek/expression.git</url>
          <connection>scm:git:git@github.com:johnynek/expression.git</connection>
        </scm>
        <developers>
          <developer>
            <id>posco</id>
            <name>Oscar Boykin</name>
            <url>http://twitter.com/posco</url>
          </developer>
        </developers>)
  )

  def module(name: String) = {
    val id = "expression-%s".format(name)
    Project(id = id, base = file(id), settings = sharedSettings ++ Seq(
      Keys.name := id)
    )
  }

  lazy val expressionCore = module("core")
  lazy val scaldingMacros = module("macros").settings(
    libraryDependencies <++= (scalaVersion) { scalaVersion => Seq(
      "org.scala-lang" % "scala-library" % scalaVersion,
      "org.scala-lang" % "scala-reflect" % scalaVersion
    ) ++ (if(isScala210x(scalaVersion)) Seq("org.scalamacros" %% "quasiquotes" % "2.0.1") else Seq())
  },
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  ).dependsOn(expressionCore)
}

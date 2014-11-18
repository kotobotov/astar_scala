name := "astar"

version := "0.1"

organization := "com.larroy"

scalaVersion := "2.10.3"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

resolvers += "version99 Empty loggers" at "http://version99.qos.ch"

libraryDependencies ++= {
  Seq(
    "org.specs2" %% "specs2" % "2.3.12" % "test",
    "com.github.scopt" %% "scopt" % "3.2.0",
    "org.slf4j" % "jcl-over-slf4j" % "1.7.7",
    "commons-logging" % "commons-logging" % "99-empty",
    "ch.qos.logback" % "logback-classic" % "1.0.13"
  )
}

testOptions in Test += Tests.Argument(TestFrameworks.Specs2, "console", "junitxml")

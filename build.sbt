name := "HMSP"

version := "0.80"

scalaVersion := "2.9.2"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

jetty()

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases",
  "Typesafe Repository Snapshots" at "http://repo.typesafe.com/typesafe/snapshots"
)

libraryDependencies ++= {
  val liftVersion = "2.5"
  Seq(
  "net.liftweb" %% "lift-webkit" % liftVersion % "compile->default",
  "net.liftweb" %% "lift-mapper" % liftVersion % "compile->default",
  "net.liftweb" %% "lift-wizard" % liftVersion % "compile->default",
  "net.liftmodules" %% "widgets" % "2.5-M1-1.1" % "compile->default"
  )
}

libraryDependencies ++= Seq(
  "cc.co.scala-reactive" %% "reactive-web" % "latest.integration" % "compile->default",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.1-seq",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.1-seq",
  "org.scalaz" %% "scalaz-core" % "6.0.4",
  "com.typesafe.akka" % "akka-actor" % "2.0",
  "com.typesafe.akka" % "akka-remote" % "2.0",
  "jfree" % "jfreechart" % "latest.integration" % "compile->default", // plotting
  "org.eclipse.jetty" % "jetty-webapp" % "8.1.7.v20120910" % "container", // For Jetty, add scope test to make jetty avl. for tests
  "junit" % "junit" % "4.8" % "test->default", // For JUnit 4 testing
  "org.specs2" %% "specs2" % "1.12.2" % "test", // For specs.org tests
  "javax.servlet" % "servlet-api" % "2.5" % "provided->default",
  "com.h2database" % "h2" % "latest.integration", // In-process database, useful for development systems
  "ch.qos.logback" % "logback-classic" % "0.9.26" % "compile->default" // Logging
)

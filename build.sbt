lazy val commonSettings = Seq(
  organization := "org.bitcoins",
  name := "bitcoin-s-core",
  scalaVersion := "2.11.7"
)

lazy val appName = "bitcoin-s-core"
lazy val scalaV = "2.11.7"
lazy val slf4jV = "1.7.5"
lazy val logbackV = "1.0.13"
lazy val scalaTestV = "3.0.5"
lazy val scalacheckV = "1.13.4"
lazy val sprayV = "1.3.2"
lazy val bouncyCastleV = "1.55"
lazy val appDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalaTestV % "test",
  "com.novocode" % "junit-interface" % "0.10" % "test",
  "org.scalacheck" %% "scalacheck" % scalacheckV withSources() withJavadoc(),

  ("org.bitcoinj" % "bitcoinj-core" % "0.14.4" % "test").exclude("org.slf4j", "slf4j-api"),
  "org.bouncycastle" % "bcprov-jdk15on" % bouncyCastleV,

  "org.slf4j" % "slf4j-api" % slf4jV % "provided",
  "ch.qos.logback" % "logback-classic" % logbackV % "test",

  "io.spray" %% "spray-json" % sprayV  % "test"
)

lazy val root = Project(appName, file(".")).enablePlugins().settings(
    commonSettings,
    libraryDependencies ++= appDependencies
)

//test in assembly := {}

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")

//testOptions in Test += Tests.Argument("-oF")

//parallelExecution in Test := false

coverageExcludedPackages := ".*gen"

coverageMinimum := 90

coverageFailOnMinimum := true

assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)

scalacOptions ++= Seq("-Xmax-classfile-name", "140")

publishTo := {
  val artifactory = "https://artifactory.service.systems.internal.projecticeland.net/artifactory/"
  if (isSnapshot.value)
    Some("Artifactory Realm" at artifactory + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  else
    Some("Artifactory Realm"  at artifactory + "libs-release-local")
}

credentials += {
  val propUsername = scala.sys.props.get("artifactory.username")
  val envUsername = scala.sys.env.get("ARTIFACTORY_USERNAME")
  val username = propUsername orElse envUsername getOrElse ""
  val propPassword = scala.sys.props.get("artifactory.password")
  val envPassword = scala.sys.env.get("ARTIFACTORY_PASSWORD")
  val password = propPassword orElse envPassword getOrElse ""
  Credentials("Artifactory Realm", "artifactory.service.systems.internal.projecticeland.net", username, password)
}

import Deps._

lazy val commonSettings = Seq(
  organization := "org.bitcoins",
  scalaVersion := "2.11.7"
)


lazy val appName = "bitcoin-s-core"

lazy val root = Project(appName, file(".")).enablePlugins().settings(
    commonSettings,
    libraryDependencies ++= Deps.root 
)

lazy val zmq = Project("bitcoin-s-zmq", file("zmq")).enablePlugins().settings(
  commonSettings,
  libraryDependencies ++= Deps.zmq
).dependsOn(root)

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

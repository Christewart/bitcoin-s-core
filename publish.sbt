
publishTo in ThisBuild := {
  val artifactory = "https://artifactory.service.systems.internal.projecticeland.net/artifactory/"
  if (isSnapshot.value)
    Some("Artifactory Realm" at artifactory + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  else
    Some("Artifactory Realm"  at artifactory + "libs-release-local")
}

credentials in ThisBuild += {
  val propUsername = scala.sys.props.get("artifactory.username")
  val envUsername = scala.sys.env.get("ARTIFACTORY_USERNAME")
  val username = propUsername orElse envUsername getOrElse ""
  val propPassword = scala.sys.props.get("artifactory.password")
  val envPassword = scala.sys.env.get("ARTIFACTORY_PASSWORD")
  val password = propPassword orElse envPassword getOrElse ""
  Credentials("Artifactory Realm", "artifactory.service.systems.internal.projecticeland.net", username, password)
}

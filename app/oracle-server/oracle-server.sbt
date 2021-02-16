name := "bitcoin-s-oracle-server"

// Ensure actor system is shut down
// when server is quit
Compile / fork := true

libraryDependencies ++= Deps.oracleServer

mainClass := Some("org.bitcoins.oracle.server.OracleServerMain")

packageSummary := "A DLC Oracle"

packageDescription := "A basic DLC oracle that allows you to commit to events and sign them"

//https://sbt-native-packager.readthedocs.io/en/latest/formats/docker.html
dockerBaseImage := "openjdk:16-jdk-buster"

packageName in Docker := packageName.value

version in Docker := version.value

dockerExposedPorts ++= Seq(9998)

dockerEntrypoint := Seq("/opt/docker/bin/bitcoin-s-oracle-server",
                        "--conf",
                        "/opt/docker/docker-application.conf")

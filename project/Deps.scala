import sbt._

object Deps {

  object V {
    val bouncyCastle = "1.55"
    val logback = "1.0.13"
    val scalacheck = "1.14.0"
    val scalaTest = "3.0.5"
    val slf4j = "1.7.5"
    val spray = "1.3.2"
    val zeromq = "0.4.3"
    val akkav = "10.1.5"
    val akkaStreamv = "2.5.17"
    val playv = "2.6.10"
    val scodecV = "1.1.6"
    val junitV = "0.11"
    val nativeLoaderV = "2.3.2"
    val typesafeConfigV = "1.3.3"

    val jodaV = "2.9.4"
    val postgresV = "9.4.1210"
    val akkaActorV = akkaStreamv
    val slickV = "3.2.3"
  }

  object Compile {
    val bouncycastle = "org.bouncycastle" % "bcprov-jdk15on" % V.bouncyCastle withSources() withJavadoc()
    val scodec = "org.scodec" %% "scodec-bits" % V.scodecV withSources() withJavadoc()
    val slf4j = "org.slf4j" % "slf4j-api" % V.slf4j % "provided" withSources() withJavadoc()
    val zeromq = "org.zeromq" % "jeromq" % V.zeromq withSources() withJavadoc()
    val akkaHttp = "com.typesafe.akka" %% "akka-http" % V.akkav withSources() withJavadoc()
    val akkaStream = "com.typesafe.akka" %% "akka-stream" % V.akkaStreamv withSources() withJavadoc()
    val akkaActor = "com.typesafe.akka" %% "akka-actor" % V.akkaStreamv withSources() withJavadoc()
    val playJson = "com.typesafe.play" %% "play-json" % V.playv withSources() withJavadoc()
    val typesafeConfig = "com.typesafe" % "config" % V.typesafeConfigV withSources() withJavadoc()

    val logback = "ch.qos.logback" % "logback-classic" % V.logback withSources() withJavadoc()

    //for loading secp256k1 natively
    val nativeLoader = "org.scijava" % "native-lib-loader" % V.nativeLoaderV withSources() withJavadoc()

    //node deps
    val joda = "joda-time" % "joda-time" % V.jodaV
    val akkaSlf4j = ("com.typesafe.akka" %% "akka-slf4j" % V.akkaStreamv withSources() withJavadoc()).exclude("org.slf4j", "slf4j-api")
    val slick = "com.typesafe.slick" %% "slick" % V.slickV withSources() withJavadoc()
    val slickHikari = "com.typesafe.slick" %% "slick-hikaricp" % V.slickV
    val postgres = "org.postgresql" % "postgresql" % V.postgresV
  }

  object Test {
    val bitcoinj = ("org.bitcoinj" % "bitcoinj-core" % "0.14.4" % "test").exclude("org.slf4j", "slf4j-api")
    val junitInterface = "com.novocode" % "junit-interface" % V.junitV % "test" withSources() withJavadoc()
    val logback = "ch.qos.logback" % "logback-classic" % V.logback % "test" withSources() withJavadoc()
    val scalacheck = "org.scalacheck" %% "scalacheck" % V.scalacheck % "test" withSources() withJavadoc()
    val scalaTest = "org.scalatest" %% "scalatest" % V.scalaTest % "test" withSources() withJavadoc()
    val spray = "io.spray" %% "spray-json" % V.spray % "test" withSources() withJavadoc()
    val akkaHttp = "com.typesafe.akka" %% "akka-http-testkit" % V.akkav % "test" withSources() withJavadoc()
    val akkaStream = "com.typesafe.akka" %% "akka-stream-testkit" % V.akkaStreamv % "test" withSources() withJavadoc()
<<<<<<< HEAD
=======

    val testkit = "org.bitcoins" %% "bitcoin-s-testkit" % V.bitcoinsV % "test" withSources() withJavadoc()
    val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % V.akkaActorV withSources() withJavadoc()
>>>>>>> Add files from old spv node project
  }

    val core = List(
      Compile.bouncycastle,
      Compile.scodec,
      Compile.slf4j
  )

  val secp256k1jni = List(
    Compile.nativeLoader,
    Test.junitInterface
  )

  val coreGen = List(
    Compile.slf4j,
    Test.scalacheck
  )

  val coreTest = List(
    Test.bitcoinj,
    Test.junitInterface,
    Test.logback,
    Test.scalaTest,
    Test.spray
  )

  val bitcoindZmq = List(
    Compile.zeromq,
    Compile.slf4j,
    Test.logback,
    Test.scalacheck,
    Test.scalaTest
  )

  val bitcoindRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j,
    Compile.typesafeConfig
  )

  val bitcoindRpcTest = List(
    Test.akkaHttp,
    Test.akkaStream,
    Test.logback,
    Test.scalaTest,
    Test.scalacheck
  )

  val bench = List(
    "org.slf4j" % "slf4j-api" % V.slf4j withSources() withJavadoc(),
    Compile.logback
  )

  val eclairRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j
  )

  val eclairRpcTest = List(
    Test.akkaHttp,
    Test.logback,
    Test.scalaTest,
    Test.scalacheck
  )

  val node = List(
    Compile.akkaActor,
    Compile.akkaSlf4j,
    Compile.joda,
    Compile.slick,
    Compile.slickHikari,
    Compile.postgres,

    Test.akkaTestkit,
    Test.logback,
    Test.scalaTest,
    Test.scalacheck,
    Test.testkit
  )

  val testkit = List(
    Compile.slf4j,
    "org.scalacheck" %% "scalacheck" % V.scalacheck withSources() withJavadoc(),
    "org.scalatest" %% "scalatest" % V.scalaTest withSources() withJavadoc()
  )

  val doc = List(
    Test.scalaTest,
    Test.logback
  )
}

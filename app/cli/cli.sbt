name := s"bitcoin-s-cli"

Universal / packageName := CommonSettings.buildPackageName((Universal /packageName).value)

libraryDependencies ++= Deps.cli.value

nativeImageJvm := "graalvm-java21"

nativeImageVersion := "21.1.0"

nativeImageOptions ++= Seq(
  "-H:+ReportExceptionStackTraces",
  "--initialize-at-build-time",
  "--no-fallback",
  "--enable-http",
  "--enable-https"
)

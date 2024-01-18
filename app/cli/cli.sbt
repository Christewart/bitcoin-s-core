name := s"bitcoin-s-cli"

Universal / packageName := CommonSettings.buildPackageName((Universal /packageName).value)

libraryDependencies ++= Deps.cli.value

nativeImageJvm := "graalvm-java17"

nativeImageVersion := "17.0.10"

nativeImageOptions ++= Seq(
  "-H:+ReportExceptionStackTraces",
  "--initialize-at-build-time",
  "--no-fallback",
  "--enable-http",
  "--enable-https"
)


name := "bitcoin-s-core-test"

libraryDependencies ++= Deps.coreTest

publishArtifact := false

testOptions in Test += Tests.Argument(
  TestFrameworks.ScalaCheck, 
  "-verbosity", "2",
  "-minSuccessfulTests", "100"
  )

testOptions in Test += Tests.Argument("-oF")

coverageExcludedPackages := ".*gen"

coverageMinimum := 90

coverageFailOnMinimum := true

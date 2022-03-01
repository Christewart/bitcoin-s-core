import com.typesafe.sbt.packager.windows._
import com.typesafe.sbt.SbtNativePackager.Windows

name := "bitcoin-s-bundle"

enablePlugins(WindowsPlugin)

mainClass := Some("org.bitcoins.bundle.gui.BundleGUI")

publish / skip := true

// Fork a new JVM for 'run' and 'test:run' to avoid JavaFX double initialization problems
fork := true

assembly / mainClass := Some("org.bitcoins.bundle.gui.BundleGUI")

assembly / assemblyJarName := s"${name.value}.jar"

//need compatibility with windows versioning scheme which is
//w.x.y.z
Windows / version := {
  val commitNumberOpt = EnvUtil.parseCommitsSinceLastTag(version.value)
  val versionStr = EnvUtil.parseVersion(version.value)
  commitNumberOpt match {
    case Some(commitNumber) => versionStr + s".${commitNumber}"
    case None => versionStr
  }
}

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _ @_*)       => MergeStrategy.discard
  case PathList("reference.conf", _ @_*) => MergeStrategy.concat
  case _                                 => MergeStrategy.first
}

// general package information (can be scoped to Windows)
maintainer := "Chris Stewart <stewart.chris1234@gmail.com>"
// Will say "Welcome to the <packageSummary> Setup Wizard"
packageSummary := "Bitcoin-S"
// Will be used for drop down menu in setup wizard
packageDescription := "Bitcoin-S"

// wix build information
wixProductId := "DC3E42C8-F6F3-4069-889E-744086E88E40"
wixProductUpgradeId := java.util.UUID.randomUUID().toString

// Adding the wanted wixFeature:
wixFeatures += WindowsFeature(
  id = "shortcuts",
  title = "Shortcuts in start menu",
  desc = "Add shortcuts for execution and uninstall in start menu",
  components = Seq(
    AddShortCuts(Seq("bin/bitcoin-s-bundle.bat"))
  )
)

// for windows class paths being too long
scriptClasspath := Seq("*")

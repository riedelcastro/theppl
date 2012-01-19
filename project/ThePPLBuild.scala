import java.io.IOException
import sbt._
import Keys._


object BuildSettings {
  val buildOrganization = "org.riedelcastro.theppl"
  val buildVersion = "0.1-SNAPSHOT"
  val buildScalaVersion = "2.9.1"

  val ieslCredentials = Credentials(Path.userHome / ".ivy2" / ".credentials")
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    shellPrompt := ShellPrompt.buildShellPrompt,
    credentials += ieslCredentials,
    publishTo <<= (version) { version: String =>
      val iesl = "http://iesl.cs.umass.edu:8081/nexus/content/repositories/"
      if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at iesl + "snapshots/")
      else                                   Some("releases"  at iesl + "releases/")
    }
  )
}

object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  def currBranch = {
    try {
    (
      ("git status -sb" lines_! devnull headOption)
        getOrElse "-" stripPrefix "## "
      )
    } catch {
      case ex:IOException => "?"
    }
  }

  val buildShellPrompt = {
    (state: State) => {
      val currProject = Project.extract (state).currentProject.id
      "%s:%s:%s> ".format (
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }
}


object ThePPLBuild extends Build {

  import BuildSettings._

  lazy val root = Project(id = "theppl",
    base = file("."),
    settings = buildSettings
  ).aggregate(core, datasets, examples)


  lazy val core = Project(id = "theppl-core",
    base = file("theppl-core"),
    settings = buildSettings
  )

  lazy val datasets = Project(id = "theppl-datasets",
    base = file("theppl-datasets"),
    settings = buildSettings
  )

  lazy val examples = Project(id = "theppl-apps",
    base = file("theppl-apps"),
    settings = buildSettings
  ) dependsOn(core, datasets)

}

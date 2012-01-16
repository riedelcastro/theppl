import sbt._

object ThePPLBuild extends Build {
  lazy val root = Project(id = "theppl",
    base = file(".")) aggregate(core, datasets, examples)

  lazy val core = Project(id = "theppl-core",
    base = file("theppl-core"))

  lazy val datasets = Project(id = "theppl-datasets",
    base = file("theppl-datasets"))

  lazy val examples = Project(id = "theppl-apps",
    base = file("theppl-apps")) dependsOn(core, datasets)

}

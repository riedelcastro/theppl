name := "theppl"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

publishTo <<= (version) { version: String =>
  val iesl = "http://iesl.cs.umass.edu:8081/nexus/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at iesl + "snapshots/")
  else                                   Some("releases"  at iesl + "releases/")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

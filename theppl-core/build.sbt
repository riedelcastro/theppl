name := "theppl-core"

scalacOptions ++= Seq("-unchecked","-deprecation")

resolvers ++= Seq(
    "IESL third party" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/thirdparty/",
    "IESL snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots",
    "IESL releases" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/releases"
)

libraryDependencies ++= Seq(
     "org.scalatest" %% "scalatest" % "1.6.1" % "test",
     "log4j" % "log4j" % "1.2.16",
     "com.typesafe" % "config" % "0.4.1",
     "org.riedelcastro.nurupo" %% "nurupo" % "0.1-SNAPSHOT"
)


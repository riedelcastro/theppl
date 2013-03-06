resolvers ++= Seq(
      "Homeniscient third party" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/thirdparty",
      "Homeniscient snapshots" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/snapshots",
      "Homeniscient releases" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/releases"
)

libraryDependencies ++= Seq(
     "org.riedelcastro.frontlets" %% "frontlets" % "0.5.0-SNAPSHOT"
)


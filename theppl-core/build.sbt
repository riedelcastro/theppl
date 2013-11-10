name := "theppl-core"

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers ++= Seq(
  "Homeniscient third party" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/thirdparty",
  "Homeniscient snapshots" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/snapshots",
  "Homeniscient releases" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/releases",
  "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze-math" % "0.3",
  "org.scalanlp" %% "breeze-learn" % "0.3",
  "org.scalanlp" %% "breeze-process" % "0.3",
  "org.scalanlp" %% "breeze-viz" % "0.3",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "log4j" % "log4j" % "1.2.16",
  "com.typesafe" % "config" % "0.4.1",
  "org.riedelcastro.nurupo" %% "nurupo" % "0.1-SNAPSHOT",
  "net.sf.trove4j" % "trove4j" % "3.0.3",
  "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided",
  "com.google.guava" % "guava" % "15.0",
  "com.google.code.findbugs" % "jsr305" % "2.0.2"
)


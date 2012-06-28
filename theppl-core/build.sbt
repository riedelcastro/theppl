name := "theppl-core"

scalacOptions ++= Seq("-unchecked","-deprecation")

libraryDependencies ++= Seq(
     "org.scalatest" %% "scalatest" % "1.6.1" % "test",
     "log4j" % "log4j" % "1.2.16",
     "com.typesafe" % "config" % "0.4.1"
)


import sbt._
import Keys._

object ThePPLBuild extends Build {
    lazy val root = Project(id = "theppl",
                            base = file(".")) aggregate(core)


    lazy val core = Project(id = "theppl-core",
                           base = file("theppl-core"))

/*  
  lazy val bar = Project(id = "hello-bar",
                           base = file("bar"))
*/
}

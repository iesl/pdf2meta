import sbt._
import edu.umass.cs.iesl.sbtbase.Dependencies
import edu.umass.cs.iesl.sbtbase.IeslProject._


object Pdf2MetaBuild extends Build {

  implicit val allDeps: Dependencies = new Dependencies(); //(CleanLogging.excludeLoggers)  // doesn't work?

  import allDeps._

  val vers = "0.1-SNAPSHOT"

  val deps = Seq(
    ieslScalaCommons("latest.integration"),
    bibmogrify("latest.integration"),
    liftJson(),
    scalatest(),
    // scalacheck(),
    classutil(),
    pdfbox("1.6.0"),
    jclOverSlf4j()
    )


  lazy val pdf2meta = Project("pdf2meta", file(".")).ieslSetup(vers, deps, Public, WithSnapshotDependencies).cleanLogging.standardLogging()

///       .ieslSetup(vers, deps, Public, WithSnapshotDependencies, org = organization, conflict = ConflictStrict)
}
 /*
libraryDependencies +=  "edu.umass.cs.iesl" %% "scalacommons" % "0.1-SNAPSHOT"  changing()

libraryDependencies +=  "edu.umass.cs.iesl" %% "bibmogrify" % "0.1-SNAPSHOT"  changing()

libraryDependencies +=  "net.liftweb" %% "lift-json" % "2.4-M5"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

libraryDependencies += "org.clapper" %% "classutil" % "0.4.3"

libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "1.6.0" exclude("commons-logging", "commons-logging")

libraryDependencies += "org.slf4j" % "jcl-over-slf4j" % "1.6.4"
*/

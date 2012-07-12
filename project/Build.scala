import sbt._
import edu.umass.cs.iesl.sbtbase.IeslProject
import edu.umass.cs.iesl.sbtbase.IeslProject._
import edu.umass.cs.iesl.sbtbase.Dependencies._

// this is just an example, to show how simple a build can be once all the boilerplate stuff is factored out.

object Pdf2MetaBuild extends Build {

  val vers = "0.1-SNAPSHOT"

  val deps = Seq(
    ieslScalaCommons("latest.integration"),
    bibmogrify("latest.integration"),
    liftJson(),
    scalatest(),
    scalacheck(),
    classutil(),
    pdfbox("1.6.0"),
    jclOverSlf4j()
    )


  lazy val pdf2meta = IeslProject("pdf2meta", vers, deps, Public, WithSnapshotDependencies)

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

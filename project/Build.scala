import com.earldouglas.xsbtwebplugin.WebPlugin
import sbt._
import sbtassembly.Plugin._
import AssemblyKeys._
import edu.umass.cs.iesl.sbtbase.Dependencies
import edu.umass.cs.iesl.sbtbase.IeslProject._
import sbt.Keys._


object Pdf2MetaBuild extends Build {

  implicit val allDeps: Dependencies = new Dependencies(); //(CleanLogging.excludeLoggers)  // doesn't work?

  import allDeps._

  val vers = "0.1-SNAPSHOT"

  val deps = Seq(
    ieslScalaCommons("latest.integration"),
    bibmogrify("latest.integration"),
    "org.json4s" %% "json4s-native" % "3.2.9",
    scalatest(),
    scalacheck(),
    classutil(),
    pdfbox("1.6.0"),
    //lift to support session
    liftWebkit(),
    liftMapper(),
    liftWizard(),
    jetty("6.1.26"),
    jettyContainer("6.1.26"),

    //jclOverSlf4j(),
    // these should be provided transitively by scalacommons, but they aren't because it's defined "notTransitive"
    dsutils(), commonsLang(), 
    "org.rogach" %% "scallop" % "0.9.5",
    "org.clapper" %% "classutil" % "1.0.4",
    "edu.umass.cs.iesl" %% "namejuggler" % "0.1-SNAPSHOT",
    "org.mongodb" %% "casbah" % "2.5.0"
  )


  lazy val pdf2meta = Project("pdf2meta", file(".")).ieslSetup(vers, deps, Public, WithSnapshotDependencies).settings(assemblySettings: _*)
    .cleanLogging.standardLogging
    .settings(scalaVersion := "2.10.4")
    .settings(mainClass in assembly := Some("edu.umass.cs.iesl.bibmogrify.BibMogrify"))
    .settings(WebPlugin.webSettings :_*)
    .settings(mergeStrategy in assembly <<= (mergeStrategy in assembly) {
    (old) => {
      case "logback.xml" => MergeStrategy.first
      case p if p.startsWith("com/typesafe/config") => MergeStrategy.first
      case p if p.startsWith("scala/") => MergeStrategy.first
      case p if p.endsWith("html") => MergeStrategy.rename
      case p if p.startsWith("META-INF") => MergeStrategy.discard
      case p if p.startsWith("org/scala_tools/time") => MergeStrategy.first
      case "messages" => MergeStrategy.discard
      // case ps @ (x :: xs) if ps.last.endsWith(".jar") => MergeStrategy.first
      case x => {
        println("assembly: "+x)
        old(x)
      }
    } })

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

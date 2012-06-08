import AssemblyKeys._ // put this at the top of the file

name := "pdf2meta"

organization := "edu.umass.cs.iesl"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies +=  "edu.umass.cs.iesl" %% "scalacommons" % "0.1-SNAPSHOT"  changing()

libraryDependencies +=  "edu.umass.cs.iesl" %% "bibmogrify" % "0.1-SNAPSHOT"  changing()

libraryDependencies +=  "net.liftweb" %% "lift-json" % "2.4-M5"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

libraryDependencies += "org.clapper" %% "classutil" % "0.4.3"

libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "1.6.0" exclude("commons-logging", "commons-logging")

libraryDependencies += "org.slf4j" % "jcl-over-slf4j" % "1.6.4"

resolvers += "David Soergel Repo" at "http://dev.davidsoergel.com/nexus/content/groups/public"

resolvers += "David Soergel Snapshots" at "http://dev.davidsoergel.com/nexus/content/repositories/snapshots"

resolvers += "IESL Repo" at "https://dev-iesl.cs.umass.edu/content/repositories/releases"

resolvers += "IESL Snapshot Repo" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots"

seq(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)

seq(assemblySettings: _*)

publishTo <<= (version)
                                            {version: String =>
                                              {
                                              def repo(name: String) = name at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/" + name
                                              val isSnapshot = version.trim.endsWith("SNAPSHOT")
                                              val repoName = if (isSnapshot) "snapshots" else "releases"
                                              Some(repo(repoName))
                                              }
                                            }

credentials +=
                                  {
                                  Seq("build.publish.user", "build.publish.password").map(k => Option(System.getProperty(k))) match
                                  {
                                    case Seq(Some(user), Some(pass)) =>
                                      Credentials("Sonatype Nexus Repository Manager", "dev-iesl.cs.umass.edu", user, pass)
                                    case _ =>
                                      Credentials(Path.userHome / ".ivy2" / ".credentials")
                                  }
                                  }

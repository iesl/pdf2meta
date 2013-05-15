import sbt._
import sbt.Keys._

object IeslPluginLoader extends Build
{

  // automatically updating SNAPSHOTs does not seem to work right here.
  // as a workaround, we'll just release every incremental change to iesl-sbt-base with integer build numbers.
  lazy val root = Project(id = "plugins", base = file("."))
    .settings(resolvers += "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public")
    //.settings(resolvers += "IESL Public Snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public-snapshots")
    .settings(addSbtPlugin("edu.umass.cs.iesl" %% "iesl-sbt-base" % "latest.release")) // apparently buggy: "latest.integration" changing()
    // Our Nexus can't proxy this Ivy-format repo
    .settings(resolvers +=
    Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver
      .ivyStylePatterns))
    .settings(addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.4"))
    .settings(addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.6.0"))
    .settings(addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0"))
}


// as of sbt 0.12.0 we can rebuild the plugin on the fly from the hg repository,
// avoiding the Nexus URL chicken-and-egg problem (or rather, pushing it back one level to the Bitbucket URL)

/*
import sbt._

object IeslPluginLoader extends Build {
  override lazy val projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn( ieslSbtBase )
  lazy val ieslSbtBase = uri("hg:ssh://bitbucket.org/IESL/iesl-sbt-base")
}
 */


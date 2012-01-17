resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

resolvers += "retronym-releases" at "http://retronym.github.com/repo/releases"

resolvers += "retronym-snapshots" at "http://retronym.github.com/repo/snapshots"

//addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "0.11.0")

//addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.7")

resolvers += "Web plugin repo" at "http://siasia.github.com/maven2"

libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-web-plugin" % (v+"-0.2.10"))

addSbtPlugin("org.scalaxb" % "sbt-scalaxb" % "0.6.6")

resolvers += "Web plugin repo" at "http://siasia.github.com/maven2"

// putting this in ~/.sbt/build.sbt doesn't work
resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

//Following means libraryDependencies += "com.github.siasia" %% "xsbt-web-plugin" % "0.1.1-<sbt version>""
libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-web-plugin" % ("0.1.1-"+v))

//libraryDependencies <+= sbtVersion("com.github.siasia" %% "xsbt-web-plugin" % _)

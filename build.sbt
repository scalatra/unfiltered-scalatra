organization := "com.example"

name := "My Web Project"

version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
   "net.databinder" %% "unfiltered-filter" % "0.5.6",
   "net.databinder" %% "unfiltered-jetty" % "0.5.6",
   "org.clapper" %% "avsl" % "0.3.1",
   "org.scalatra" %% "scalatra" % "2.1.0-SNAPSHOT"
)

resolvers ++= Seq(
  "java m2" at "http://download.java.net/maven/2",
  "sonatype oss snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

import sbt.Keys.scalaVersion

name := "DuplicateImageIdentifier"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.sksamuel.scrimage" % "scrimage-core_2.12" % "2.1.8"

libraryDependencies += "com.sksamuel.scrimage" % "scrimage-io-extra_2.12" % "2.1.8"

libraryDependencies += "com.sksamuel.scrimage" % "scrimage-filters_2.12" % "2.1.8"
libraryDependencies += "io.spray" %% "spray-json" % "1.3.3"

// https://mvnrepository.com/artifact/com.github.jbellis/jamm
libraryDependencies += "com.github.jbellis" % "jamm" % "0.2.6"

def makeAgentOptions(classpath: Classpath): String = {
  val jammJar = classpath.map(_.data).filter(_.toString.contains("jamm")).head
  s"-javaagent:$jammJar"
}

javaOptions in Compile <+= (dependencyClasspath in Compile).map(makeAgentOptions)
fork := true
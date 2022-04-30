name := "ad-libs"

version := "0.1"

scalaVersion := "3.1.2"

// https://mvnrepository.com/artifact/org.slf4j/slf4j-api
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.8.0-beta4"
// https://mvnrepository.com/artifact/org.slf4j/slf4j-simple
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.8.0-beta4" % Compile

// extJWNL and WordNet 3.1
libraryDependencies += "net.sf.extjwnl" % "extjwnl" % "2.0.5"
libraryDependencies += "net.sf.extjwnl" % "extjwnl-data-wn31" % "1.2"

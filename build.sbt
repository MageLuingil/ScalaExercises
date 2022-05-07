ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := "3.1.2"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaExercises"
  )

lazy val AdLibParser = (project in file("AdLibParser"))

lazy val CharSequenceBuilder = (project in file("CharSequenceBuilder"))

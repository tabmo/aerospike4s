import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.tabmo",
      scalaVersion := "2.12.3",
      version := "0.1.0-SNAPSHOT"
    )),

    scalacOptions ++= Seq(
      "-deprecation", // Warn when deprecated API are used
      "-feature", // Warn for usages of features that should be importer explicitly
      "-unchecked", // Warn when generated code depends on assumptions
      "-Ywarn-dead-code", // Warn when dead code is identified
      "-Ywarn-numeric-widen", // Warn when numeric are widened
      "-Xlint", // Additional warnings (see scalac -Xlint:help)
      "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receive
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-language:existentials",
      "-language:higherKinds",
      "-language:experimental.macros"
    ),

    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),

    organization := "io.tabmo",
    name := "aerospike4s",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "0.5",
    libraryDependencies += "com.aerospike" % "aerospike-client" % "4.0.8",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
    libraryDependencies += "org.reactivestreams" % "reactive-streams" % "1.0.2",

    /*
    * Publish to tabmo organization on bintray
    */
    licenses += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
    bintrayOrganization := Some("tabmo")


    /*libraryDependencies ++= Seq(
      "com.whisk" %% "docker-testkit-scalatest" % "0.9.5" % "test",
      "com.whisk" %% "docker-testkit-impl-spotify" % "0.9.5" % "test"),

    libraryDependencies += "org.typelevel" %% "discipline" % "0.7.3" % "test"*/
  )

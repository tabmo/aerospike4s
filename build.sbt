import Dependencies._



import Dependencies._


organization in ThisBuild := "io.tabmo"
scalaVersion in ThisBuild := "2.12.5"


val commonSettings = Seq(

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
  libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF",
  libraryDependencies += "com.aerospike" % "aerospike-client" % "4.0.8",
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
)

lazy val publishSettings = Seq(
  /*
* Publish to tabmo organization on bintray
*/
  licenses += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
  bintrayOrganization := Some("tabmo")
)


lazy val aeropike4s = project
  .in(file("."))
  .settings(
    publishSettings,
    name := "aerospike4s"
  )
  .aggregate(
    core,
    stream
  )

lazy val core =
  (project in file("aerospike4s"))
    .settings(
      publishSettings,
      commonSettings,
      name := "aerospike4s-core"
    )


lazy val stream =
  (project in file("aerospike4s-akka-stream"))
    .dependsOn(core)
    .settings(
      publishSettings,
      commonSettings,
      libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.11",
      name := "aerospike4s-akka-stream"
    )

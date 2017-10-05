import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
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

    name := "Hello",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF",
    //libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.0-MF",
    libraryDependencies += "com.aerospike" % "aerospike-client" % "4.0.8",
    libraryDependencies += "io.netty" % "netty-handler" % "4.1.11.Final",
    libraryDependencies += "io.netty" % "netty-transport" % "4.1.11.Final",
    libraryDependencies += "io.netty" % "netty-transport-native-epoll" % "4.1.11.Final",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",

    libraryDependencies ++= Seq(
      "com.whisk" %% "docker-testkit-scalatest" % "0.9.5" % "test",
      "com.whisk" %% "docker-testkit-impl-spotify" % "0.9.5" % "test"),

    libraryDependencies += "org.typelevel" %% "discipline" % "0.7.3" % "test"
)

import Dependencies._

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
addCompilerPlugin(("org.scalameta" % "paradise" % "3.0.0-M11").cross(CrossVersion.full))

lazy val `scala-with-cats` = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.8",
      version := "0.1.0-SNAPSHOT"
    )),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.6.0",
      "org.typelevel" %% "cats-mtl-core" % "0.4.0",
      "org.typelevel" %% "cats-tagless-macros" % "0.2.0",
      "io.monix" %% "monix" % "3.0.0-RC2",
      scalaTest % Test
    ),
    scalacOptions ++= Seq(
      "-Ypartial-unification"
//      , "-Xfatal-warnings"
    )
  )

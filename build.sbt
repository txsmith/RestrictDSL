
name := "RestrictDSL"
version := "0.1"

val catsVersion = "1.5.0"
val shapelessVersion = "2.3.3"
val drosteVersion = "0.7.0"
val souceCodeVersion = "0.1.5"
val circeVersion = "0.10.0"
val scalaTestVersion = "3.0.5"
val scalaCheckVersion = "1.14.0"
val scalaLangVersion = "2.12.8"
val akkaVersion = "2.5.23"

val scalaMacrosVersion = "2.0.0-96-9f738df2"

cancelable in Global := true

val commonSettings = Seq(
  scalaVersion := scalaLangVersion,
  scalacOptions ++= Seq(
    "-Ypartial-unification",
    "-feature",
    "-deprecation",
    "-unchecked"
  ),
  resolvers ++= Seq(
    Resolver.bintrayRepo("scalamacros", "maven"),
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    "boundlessgeo" at "https://repo.boundlessgeo.com/main"
  ),

  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),

  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % scalaTestVersion % "test",
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
    "com.lihaoyi" %% "sourcecode" % souceCodeVersion,
    "org.typelevel" %% "cats-core" % catsVersion,
    "com.chuusai" %% "shapeless" % shapelessVersion,
    "io.higherkindness" %% "droste-core" % drosteVersion
  ),
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion),

  excludeDependencies += ExclusionRule("jgridshift", "jgridshift")
)

val prontoSettings = Seq(
  resolvers += "Pronto" at "https://rll.jfrog.io/rll/pronto/",

  libraryDependencies ++= Seq(
    "org.typelevel" %% "squants" % "1.4.0",
    "com.portofrotterdam.pronto" %% "pronto-kafka" % "0.16.2",
    "com.portofrotterdam.pronto" %% "pronto-domain" % "latest.release",
  ),

  credentials += (sys.env.get("ARTIFACT_REPO_USER") match {
    case Some(repoUser) => Credentials("Artifactory Realm", "rll.jfrog.io", repoUser, sys.env("ARTIFACT_REPO_PASSWORD"))
    case _ => Credentials(Path.userHome / ".ivy2" / ".pronto-credentials")
  })
)

lazy val macros = (project in file("macros"))
  .settings(commonSettings)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaLangVersion
  )

lazy val restrictionTypes = (project in file("types"))
  .settings(commonSettings)
  .settings(prontoSettings)
  .settings(
    name := "restriction-types",
    version := "0.3.0",
    publishTo := {
      val repoUrl = sys.env.getOrElse("ARTIFACT_REPO_URL", "https://rll.jfrog.io/rll")
      if (isSnapshot.value)
        Some("Artifactory Realm" at repoUrl + "/pronto-snapshots")
      else
        Some("Artifactory Realm" at repoUrl + "/pronto-releases")
    }
  )

lazy val core = (project in file("core"))
  .dependsOn(macros)
  .dependsOn(restrictionTypes)
  .settings(commonSettings)
  .settings(prontoSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "squants" % "1.4.0",
      "com.lihaoyi" %% "pprint" % "0.5.3",
      "io.reactivex" %% "rxscala" % "0.26.5",
      "org.bitbucket.inkytonik.kiama" % "kiama_2.12" % "2.2.0",
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      "com.typesafe.play" %% "play-json" % "2.6.8",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.slf4j" % "slf4j-api" % "1.7.26",
      "net.team2xh" %% "onions" % "1.0.1",
      "com.lihaoyi" %% "requests" % "0.1.7",
      "org.geotools" % "gt-api" % "19.1" exclude("javax.media", "jai_core"),
      "org.geotools" % "gt-epsg-hsql" % "19.1" exclude("javax.media", "jai_core"),
      "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test),
  )

lazy val portmapsInterop = (project in file("portmaps-interop"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(prontoSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.opencsv" % "opencsv" % "4.5"
    )
  )

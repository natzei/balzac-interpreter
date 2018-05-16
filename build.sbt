name := "balzac-interpreter"

version := "0.1"

scalaVersion := "2.12.6"

resolvers += Resolver.mavenLocal
resolvers += Resolver.url("scala-bitcoin-lib", url("https://oss.sonatype.org/content/repositories/snapshots/"))
//resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "fr.acinq" %% "bitcoin-lib" % "0.9.17"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "com.sulacosoft" % "BitcoindConnector4J" % "0.16.0"

//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")

lazy val akkaVersion = "2.5.12"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
)
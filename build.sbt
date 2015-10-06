name := """hzip"""

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val scalazVersion = "7.1.3"
val scalazCore = "org.scalaz" %% "scalaz-core" % scalazVersion


// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.2.4" % "test",
	"com.chuusai" %% "shapeless" % "2.2.5",
  scalazCore
)

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"


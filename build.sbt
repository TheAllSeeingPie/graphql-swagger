name := "graphql-swagger"

version := "0.1"

scalaVersion := "2.12.5"

val finagleVersion = "18.1.0"
val json4sVersion = "3.5.1"
val bijectionVersion = "0.9.6"

libraryDependencies ++= Seq(
  // GraphQL
  "org.sangria-graphql" %% "sangria" % "1.1.0",
  "org.sangria-graphql" %% "sangria-json4s-jackson" % "1.0.0",

  // Finagle and stuff
  "com.twitter" %% "finagle-http" % finagleVersion,
  "com.twitter" %% "bijection-core" % bijectionVersion,
  "com.twitter" %% "bijection-util" % bijectionVersion,

  // Testing
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)
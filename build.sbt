name := "scala-playground"

version := "0.4"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq (
  "net.team2xh"   %% "onions"     % "1.0.1",
  "org.scalatest" %% "scalatest"  % "3.0.1"     % "test"
)

mainClass := Some("com.scalaplayground.dungeonexplore.Game.Game")

test in assembly := {}


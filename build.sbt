name := "scala-playground"

version := "0.4"

scalaVersion := "2.13.0"
//crossScalaVersions := Seq("2.11.8","2.11.12", "2.12.4")

libraryDependencies ++= Seq (
  "net.team2xh"    %% "onions"         % "1.0.2-SNAPSHOT",
  "org.scalatest"  %% "scalatest"      % "3.2.0"   % "test",
  "org.mockito"    %% "mockito-scala"    % "1.15.0"  % "test"
)

mainClass := Some("com.scalaplayground.dungeonexplore.Game.Game")

test in assembly := {}

resolvers += DefaultMavenRepository



//"Maven" at "https://mvnrepository.com/artifact/"


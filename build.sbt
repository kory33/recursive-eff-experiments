scalaVersion := "3.3.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Ykind-projector:underscores"
)

libraryDependencies ++= Seq(
  "org.atnos" %% "eff" % "7.0.1",
  "org.typelevel" %% "cats-effect" % "3.5.2"
)

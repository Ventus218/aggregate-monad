val scala3Version = "3.7.4"
val catsVersion = "2.13.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aggregate-monad",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion,
    libraryDependencies += "org.typelevel" %% "cats-kernel" % catsVersion,
    libraryDependencies += "org.typelevel" %% "cats-free" % catsVersion
  )

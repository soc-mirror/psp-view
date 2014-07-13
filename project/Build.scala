import sbt._
import Keys._

object BuildSettings {
  val paradiseVersion = "2.1.0-M1"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization   := "de.oxnrtr.psp",
    version        := "0.1.2-SNAPSHOT",
    scalacOptions ++= Seq("-deprecation"),
    scalaVersion   := "2.11.1",
    exportJars     := true,
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object MyBuild extends Build {
  import BuildSettings._

  testFrameworks += new TestFramework("utest.runner.JvmFramework")

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in common)
  ) aggregate (common, query, querymacros)

  lazy val common: Project = Project(
    "common",
    file("common"),
    settings = buildSettings ++ Seq(
      name                       := "psp-view-common",
      description                := "psp alternate view implementation",
      homepage                   := Some(url("https://github.com/paulp/psp-view")),
      licenses                   := Seq("Apache" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
      parallelExecution in Test  := false,
      fork in Test               := true,
      initialCommands in console := s"cat ${baseDirectory.value}/src/main/resources/replStartup.scala".!!,
      libraryDependencies       ++= Seq(
        "org.scala-lang"  % "scala-compiler"  % scalaVersion.value,
        "jline"           % "jline"           %       "2.12",
        "ch.qos.logback"  % "logback-classic" %      "1.1.2",
        "org.scalacheck" %% "scalacheck"      %      "1.11.4"       % "test",
        "com.lihaoyi"    %% "utest"           %       "0.1.8"       % "test")
    )
  )

  lazy val query: Project = Project(
    "query",
    file("query"),
    settings = buildSettings ++ Seq(
      name                   := "psp-query")
    ) dependsOn (common, querymacros)

  lazy val querymacros: Project = Project(
    "query-macros",
    file("query-macros"),
    settings = buildSettings ++ Seq(
      name                   := "psp-query-macros",
      libraryDependencies   ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
    )
  ) dependsOn common
}


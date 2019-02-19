import Dependencies._

// A good example of build.sbt https://github.com/lift/framework/blob/master/build.sbt
inThisBuild(
  Seq(
    organization := "org.asarkar",
    version := "1.0.0",
    scalaVersion := "2.12.7",
    scalacOptions ++= Seq(
      "-unchecked",
      "-feature",
      //  "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      //  "-language:postfixOps",
      "-deprecation",
      "-encoding",
      "utf8"
    ),
    libraryDependencies ++= allDeps
  )
    ++ inConfig(Test)(Seq(
    testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-e"),
    logBuffered := false,
    fork := true,
    parallelExecution := false,
    javaOptions ++= Seq("-ea")
  ))
)

// http://www.wartremover.org/doc/warts.html
Compile / wartremoverWarnings ++= Warts.unsafe
  .filterNot(Set(Wart.NonUnitStatements).contains)

lazy val `coding-interview` = (project in file("."))
  .aggregate(
    `test-util`
  )

lazy val `test-util` = project

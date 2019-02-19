import sbt._

object Dependencies {
  lazy val scalatestVersion = "3.0.5"
  lazy val scalaticVersion = "3.0.5"
  lazy val scalacheckVersion = "1.14.0"
  lazy val scalaGraphVersion = "1.12.5"

  // compile
  val scalatic = "org.scalactic" %% "scalactic" % scalaticVersion
  // test
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion % "test"
  val scalagraph = "org.scala-graph" %% "graph-core" % scalaGraphVersion

  val allDeps = Seq(scalatic, scalatest, scalacheck, scalagraph)
}

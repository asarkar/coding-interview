import sbt._

object Dependencies {
  lazy val scalatestVersion = "3.0.5"
  lazy val scalaticVersion = "3.0.5"
  lazy val scalacheckVersion = "1.14.0"
  lazy val jsoupVersion = "1.11.3"

  // compile
  val scalatic = "org.scalactic" %% "scalactic" % scalaticVersion
  // test
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion % "test"
  val jsoup = "org.jsoup" % "jsoup" % jsoupVersion % "test"

  val allDeps = Seq(scalatic, scalatest, scalacheck, jsoup)
}

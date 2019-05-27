package org.asarkar.codinginterview.strings

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class FileReaderSpec extends FlatSpec {
  "FileReader" should "read 7 char at a time" in {
    val fr = new FileReader("Hello world")
    Iterator.continually(fr.read7())
      .take(3)
      .toList should contain theSameElementsInOrderAs Seq("Hello w", "orld", "")
  }

  it should "read 4 char at a time" in {
    val fr = new FileReader("Hello world")
    Iterator.continually(fr.readN(4))
      .takeWhile(_.nonEmpty)
      .toList should contain theSameElementsInOrderAs Seq("Hell", "o wo", "rld")
  }

  it should "read 8 char at a time" in {
    val fr = new FileReader("Hello world")
    Iterator.continually(fr.readN(8))
      .takeWhile(_.nonEmpty)
      .toList should contain theSameElementsInOrderAs Seq("Hello wo", "rld")
  }
}

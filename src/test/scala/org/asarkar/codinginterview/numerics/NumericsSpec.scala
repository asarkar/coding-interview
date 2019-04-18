package org.asarkar.codinginterview.numerics

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class NumericsSpec extends FlatSpec {
  "numerics" should "compute first n Hamming numbers" in {
    hamming(15).map(_.toInt) should contain theSameElementsInOrderAs Seq(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24)
  }

  it should "find the square root" in {
    sqrt(9) shouldBe (3d +- 0.001d)
    sqrt(2) shouldBe (1.414d +- 0.001d)
  }

  it should "compute a large sum" in {
    largeSum("946483215", "64586") shouldBe "946547801"
  }

  it should "compute PI up to 3 decimal places" in {
    val actual = 3.141d
    val error = 0.0001d
    pi(actual, error) shouldBe actual +- error
  }

  it should "pick a random element from the stream" in {
    randomNumFromStream((1 to 4).toIterator)
      .foreach(println)
  }

  it should "compute the n-th perfect number" in {
    perfectNum(2) shouldBe 28
    perfectNum(11) shouldBe 118
  }
}

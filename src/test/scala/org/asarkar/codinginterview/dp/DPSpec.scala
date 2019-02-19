package org.asarkar.codinginterview.dp

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class DPSpec extends FlatSpec {
  "dp" should "find the number of subsets of the given array that sum to k" in {
    numSubsetsOfSumK(IndexedSeq(2, 4, 6, 10), 16) shouldBe 2
  }

  it should "buy/sell stocks with n transactions to maximize profit" in {
    stocks3(IndexedSeq(2, 5, 7, 1, 4, 3, 1, 3), 3) shouldBe 10
  }
}

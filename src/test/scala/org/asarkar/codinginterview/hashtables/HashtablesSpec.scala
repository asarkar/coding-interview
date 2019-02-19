package org.asarkar.codinginterview.hashtables

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class HashtablesSpec extends FlatSpec {
  "hashtables" should "return 2-sum" in {
    val xs = IndexedSeq(0, -1, 1, 2, 3, -2)

    Map(1 -> Seq((-1, 2), (-2, 3), (0, 1)), 2 -> Seq((0, 2), (-1, 3)), -2 -> Seq((-2, 0)))
      .foreach(x => twoSum(xs, x._1) should contain theSameElementsAs x._2)
  }

  it should "return 3-sum" in {
    threeSum(IndexedSeq(19, 3, 7, 10, 11), 18) shouldBe empty
    threeSum(IndexedSeq(19, 3, 7, 10, 11), 20) should contain theSameElementsAs Seq((3, 7, 10))
    threeSum(IndexedSeq(4, 5, 6), 12) shouldBe empty
    threeSum(IndexedSeq(4, 4, 6), 12) shouldBe empty
  }
}

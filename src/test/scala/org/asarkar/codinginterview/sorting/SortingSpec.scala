package org.asarkar.codinginterview.sorting

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class SortingSpec extends FlatSpec {
  "sorting" should "return 2-sum" in {
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

  it should "return 4-sum" in {
    fourSum(IndexedSeq(1, 0, -1, 0, -2, 2), 0) should contain theSameElementsAs Seq((-2, -1, 1, 2))
  }

  it should "find the minimum number of rooms required" in {
    minRooms(Seq(
      (30, 75), (0, 50), (60, 150)
    )) shouldBe 2
    minRooms(Seq(
      (900, 910), (940, 1200), (950, 1120), (1100, 1130), (1500, 1900), (1800, 2000)
    )) shouldBe 3
  }

  it should "partition RGB array" in {
    val colors = Array('G', 'B', 'R', 'R', 'B', 'R', 'G')
    partitionRGB(colors)
    colors shouldBe colors.sorted.reverse
  }

  it should "count inversions" in {
    numInversions(IndexedSeq(2, 4, 1, 3, 5)) shouldBe 3
    numInversions(IndexedSeq(5, 4, 3, 2, 1)) shouldBe 10
  }

  it should "pancake sort" in {
    val a = Array(3, 2, 4, 1)
    pancakeSort(a)
    a shouldBe sorted

    val b = Array(1, 2, 3)
    pancakeSort(b)
    b shouldBe sorted
  }

  it should "find the smallest set of numbers that covers all the intervals" in {
    smallestCover(Seq((0, 4), (1, 2), (5, 7), (6, 7), (6, 9), (8, 10))) should contain theSameElementsInOrderAs Seq(2, 7, 10)
    smallestCover(Seq((0, 3), (2, 6), (3, 4), (6, 10))) should contain theSameElementsInOrderAs Seq(3, 10)
  }
}

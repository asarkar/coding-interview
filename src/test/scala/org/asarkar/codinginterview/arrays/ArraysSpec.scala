package org.asarkar.codinginterview.arrays

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class ArraysSpec extends FlatSpec with TableDrivenPropertyChecks {
  "arrays" should "count negative integers" in {
    countNegative(IndexedSeq(
      IndexedSeq(-3, -2, -1, 1),
      IndexedSeq(-2, 2, 3, 4),
      IndexedSeq(4, 5, 7, 8)
    )) shouldBe 4
  }

  it should "randomly reorder array" in {
    val original = Array(1, 2, 3, 4, 5)
    val xs = Array.ofDim[Int](original.length)
    Array.copy(original, 0, xs, 0, original.length)
    shuffle[Int](xs)
    xs.deep == original.deep should be
    false
  }

  it should "determine if the given sequence of towers are hoppable" in {
    isHoppable(IndexedSeq(4, 2, 0, 0, 2, 0)) shouldBe true
    isHoppable(IndexedSeq(1, 0)) shouldBe false
  }

  it should "find the intersection of two arrays" in {
    intersection(IndexedSeq(1, 3, 4, 5, 7), IndexedSeq(2, 3, 5, 6)) should contain theSameElementsAs Seq(3, 5)
    intersection(IndexedSeq(2, 5, 6), IndexedSeq(4, 6, 8, 10)) should contain theSameElementsAs Seq(6)
  }

  it should "determine if one array is a rotation of another" in {
    isRotation(1 to 5, IndexedSeq(3, 4, 5, 1, 2)) shouldBe true
    isRotation(1 to 5, IndexedSeq(3, 4, 5, 6, 7)) shouldBe false
    isRotation(1 to 5, IndexedSeq(3, 4, 5)) shouldBe false
  }

  it should "rotate an array to the right" in {
    val xs = Array(1, 2, 3, 4, 5, 6)
    rotateRight(xs, 3)
    xs.deep shouldBe Array(4, 5, 6, 1, 2, 3).deep

    val ys = Array(1, 2, 3, 4)
    rotateRight(ys, 3)
    ys.deep shouldBe Array(2, 3, 4, 1).deep
    rotateRight(ys, 1)
    ys.deep shouldBe Array(1, 2, 3, 4).deep
    rotateRight(ys, 2)
    ys.deep shouldBe Array(3, 4, 1, 2).deep
  }

  import Move._

  it should "decide if there's a winner in a Tic-Tac-Toe game" in {
    val data = Table(
      ("board", "winner"),
      (IndexedSeq(
        IndexedSeq(X, O, Empty),
        IndexedSeq(O, X, Empty),
        IndexedSeq(O, X, X)
      ), Some(X)),
      (IndexedSeq(
        IndexedSeq(O, X, Empty),
        IndexedSeq(O, X, Empty),
        IndexedSeq(O, Empty, X)
      ), Some(O)),
      (IndexedSeq(
        IndexedSeq(Empty, O, Empty),
        IndexedSeq(O, O, Empty),
        IndexedSeq(X, X, X)
      ), Some(X)),
      (IndexedSeq(
        IndexedSeq(X, O, Empty),
        IndexedSeq(O, X, Empty),
        IndexedSeq(O, X, Empty)
      ), None)
    )

    forAll(data) { (board, winner) =>
      ticTacToeWinner(board) shouldBe winner
    }

    assertThrows[IllegalArgumentException] {
      ticTacToeWinner(IndexedSeq(
        IndexedSeq(X, O, Empty),
        IndexedSeq(O, X, Empty),
        IndexedSeq(O, X, O)
      ))
    }
  }

  it should "traverse a matrix in spiral order" in {
    matrixSpiral(IndexedSeq(
      1 to 3,
      4 to 6,
      7 to 9
    )) should contain theSameElementsInOrderAs Seq(1, 2, 3, 6, 9, 8, 7, 4, 5)
    matrixSpiral(IndexedSeq(
      1 to 3
    )) should contain theSameElementsInOrderAs Seq(1, 2, 3)
    matrixSpiral(IndexedSeq(
      IndexedSeq(1),
      IndexedSeq(4)
    )) should contain theSameElementsInOrderAs Seq(1, 4)
    matrixSpiral(IndexedSeq(
      IndexedSeq(1)
    )) should contain theSameElementsInOrderAs Seq(1)
  }

  it should "find the lengths of video segments" in {
    segmentLengths(Seq('a', 'b', 'c', 'd')) should contain theSameElementsInOrderAs Seq.fill(4)(1)
    segmentLengths(Seq('a', 'b', 'c', 'a')) should contain theSameElementsAs Seq(4)
    segmentLengths(Seq('a', 'b', 'c', 'a', 'b', 'd', 'e')) should contain theSameElementsInOrderAs Seq(5, 1, 1)
  }

  it should "compute the maximum values of each subarray of length k" in {
    val data = Table(
      ("xs", "k", "maximums"),
      (IndexedSeq(11, -2, 5, 6, 0, 9, 8, -1, 2, 15), 3, Seq(11, 6, 6, 9, 9, 9, 8, 15)),
      (IndexedSeq(10, 5, 2, 7, 8, 7), 3, Seq(10, 7, 8, 8)),
      (IndexedSeq(500), 1, Seq(500)),
      (IndexedSeq(1, 1, 1), 1, Seq(1, 1, 1)),
      (IndexedSeq(6), 2, Seq.empty[Int]),
      (IndexedSeq(6), 0, Seq.empty[Int]),
      (IndexedSeq(1, -1), 1, Seq(1, -1)),
      (IndexedSeq(1, 3, 1, 2, 0, 5), 3, Seq(3, 3, 2, 5)),
      (IndexedSeq.empty[Int], 0, Seq.empty[Int]),
      (IndexedSeq(5, 3, 4), 1, IndexedSeq(5, 3, 4))
    )

    forAll(data) { (xs, k, maximums) =>
      maxValuesOfSubarrays(xs, k) should contain theSameElementsInOrderAs maximums
    }
  }

  it should "find unsorted subarray length" in {
    val data = Table(
      ("xs", "len"),
      (IndexedSeq.empty[Int], 0),
      (IndexedSeq(1), 0),
      (IndexedSeq(1, 2, 3, 4), 0),
      (IndexedSeq(1, 3, 2, 2, 2), 4),
      (IndexedSeq(2, 1), 2),
      (IndexedSeq(1, 1), 0),
      (IndexedSeq(2, 6, 4, 8, 10, 9, 15), 5),
      (IndexedSeq(1, 3, 2, 3, 3), 2)
    )

    forAll(data) { (xs, len) =>
      unsortedSubarrayLen(xs) shouldBe len
    }
  }

  it should "count the smaller numbers after self" in {
    val data = Table(
      ("xs", "ys"),
      (IndexedSeq(5, 2, 6, 1), IndexedSeq(2, 1, 1, 0)),
      (IndexedSeq(-1, -1), IndexedSeq(0, 0)),
      (IndexedSeq(1), IndexedSeq(0))
    )

    forAll(data) { (xs, ys) =>
      countSmaller(xs) should contain theSameElementsInOrderAs ys
    }
  }

  it should "count the number of pairs of bishops that attack each other" in {
    numAttackingBishops(IndexedSeq((0, 0), (1, 2), (2, 2), (4, 0), (4, 4)), 5) shouldBe 4
  }

  it should "compute the maximum product of three numbers" in {
    largestProduct(IndexedSeq(-10, -3, 5, 6, -20)) shouldBe 1200
    largestProduct(IndexedSeq(1, -4, 3, -6, 7, 0)) shouldBe 168
  }

  it should "find the longest increasing subsequence" in {
    longestIncSubseq(IndexedSeq(1, 3, 6, 7, 9, 4, 10, 5, 6)) shouldBe 6
    longestIncSubseq(IndexedSeq(2, 2)) shouldBe 1
  }

  it should "determine the minimum number of columns that can be removed" in {
    minColToRemove(IndexedSeq(
      IndexedSeq('c', 'b', 'a'),
      IndexedSeq('d', 'a', 'f'),
      IndexedSeq('g', 'h', 'i')
    )) shouldBe 1

    minColToRemove(IndexedSeq(
      IndexedSeq('a', 'b', 'c', 'd', 'e', 'f')
    )) shouldBe 0

    minColToRemove(IndexedSeq(
      IndexedSeq('z', 'y', 'x'),
      IndexedSeq('w', 'v', 'u'),
      IndexedSeq('t', 's', 'r')
    )) shouldBe 3

    minColToRemove(IndexedSeq(
      IndexedSeq('a', 'b', 'd', 'f', 'c', 'e')
    )) shouldBe 0
  }

  it should "determine whether the array could become non-decreasing by modifying at most 1 element" in {
    checkPossibility(IndexedSeq(4, 2, 1)) shouldBe false
    checkPossibility(IndexedSeq(4, 2, 3)) shouldBe true
    checkPossibility(IndexedSeq(3, 4, 2, 3)) shouldBe false
    checkPossibility(IndexedSeq(-1, 4, 2, 3)) shouldBe true
  }

  it should "determine if it's possible to reach the last index" in {
    canJump(Seq(2, 0, 1, 0)) shouldBe true
    canJump(Seq(1, 1, 0, 1)) shouldBe false
    canJump(Seq(2, 3, 1, 1, 4)) shouldBe true
    canJump(Seq(3, 2, 1, 0, 4)) shouldBe false
    canJump(Seq.empty) shouldBe true
    canJump(Seq(1)) shouldBe true
  }

  it should "square the elements and give the output in sorted order" in {
    sortedSquares(Seq(-1, 2, 2)) should contain theSameElementsInOrderAs Seq(1, 4, 4)
    sortedSquares(Seq(-3, -2, -1)) should contain theSameElementsInOrderAs Seq(1, 4, 9)
    sortedSquares(Seq(-4, -1, 0, 3, 10)) should contain theSameElementsInOrderAs Seq(0, 1, 9, 16, 100)
    sortedSquares(Seq(-7, -3, 2, 3, 11)) should contain theSameElementsInOrderAs Seq(4, 9, 9, 49, 121)
    sortedSquares(Seq.empty[Int]) shouldBe empty
    sortedSquares(Seq(1)) should contain theSameElementsAs Seq(1)
  }
}

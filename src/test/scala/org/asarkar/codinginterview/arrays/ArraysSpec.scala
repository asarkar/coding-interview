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
      (IndexedSeq(6), 0, Seq.empty[Int])
    )

    forAll(data) { (xs, k, maximums) =>
      maxValuesOfSubarrays(xs, k) should contain theSameElementsInOrderAs maximums
    }
  }
}

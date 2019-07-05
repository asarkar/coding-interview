package org.asarkar.codinginterview.bintree

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.JavaConverters._

class BintreeSpec extends FlatSpec with TableDrivenPropertyChecks {
  "bintree" should "do a Morris inorder traversal" in {
    val xs = Array[Integer](1, 2, 3, 4, 5, 6)
    val root = new Node[Integer](xs)
    BinTrees.morrisInorder(root).asScala should contain theSameElementsInOrderAs Seq(4, 2, 5, 1, 6, 3)

    val negativeOne = new Node[Integer](null, null, -1)
    val two = new Node[Integer](negativeOne, null, 2)
    val negativeTwo = new Node[Integer](null, two, -2)

    val eight = new Node[Integer](null, null, 8)
    val six = new Node[Integer](null, eight, 6)
    val five = new Node[Integer](negativeTwo, six, 5)

    val forty = new Node[Integer](null, null, 40)
    val thirty = new Node[Integer](null, forty, 30)

    val ten = new Node[Integer](five, thirty, 10)

    BinTrees.morrisInorder(ten).asScala should contain theSameElementsInOrderAs Seq(-2, -1, 2, 5, 6, 8, 10, 30, 40)
  }

  it should "build a BST from the given array" in {
    val root = BinTrees.buildBST[Integer](Array[Integer](2, 1, 3, 4))
    root should not be null
    root.left should not be null
    root.right should not be null
    root.left.left shouldBe null
    root.left.right shouldBe null
    root.right.left shouldBe null
    root.right.right should not be null
    root.datum shouldBe 2
    root.left.datum shouldBe 1
    root.right.datum shouldBe 3
    root.right.right.datum shouldBe 4
  }

  it should "do a level order traversal" in {
    val root = BinTrees.buildBST[Integer](Array[Integer](2, 1, 3, 4))
    BinTrees.levelOrder(root).asScala should contain theSameElementsInOrderAs Seq(2, 1, 3, 4)
  }

  it should "serialize and deserialize a binary tree" in {
    // src/test/resources/serialized-bst.png
    val six = new Node[Integer](null, null, 6)
    val four = new Node[Integer](six, null, 4)
    val five = new Node[Integer](null, null, 5)
    val two = new Node[Integer](four, five, 2)
    val three = new Node[Integer](null, null, 3)
    val one = new Node[Integer](two, three, 1)

    serialize(one) shouldBe "1 2 4 6 # # # 5 # # 3 # #"
    val root = deserialize(serialize(one))
    root should not be null
    root.datum shouldBe 1
    root.left should not be null
    root.right should not be null
    root.left.datum shouldBe 2
    root.right.datum shouldBe 3
    root.right.left shouldBe null
    root.right.right shouldBe null
    root.left.left should not be null
    root.left.right should not be null
    root.left.left.datum shouldBe 4
    root.left.right.datum shouldBe 5
    root.left.left.left should not be null
    root.left.left.right shouldBe null
    root.left.left.left.datum shouldBe 6
    root.left.left.left.left shouldBe null
  }

  // c.f. unival.png
  it should "count unival trees" in {
    val b1 = new Node[Character](null, null, 'b')
    val b2 = new Node[Character](null, b1, 'b')
    val b3 = new Node[Character](null, null, 'b')
    val b4 = new Node[Character](b3, b2, 'b')
    val c = new Node[Character](null, null, 'c')
    val a = new Node[Character](c, b4, 'a')
    countUnival(a) shouldBe 5

    val A = new Node[Character](null, null, 'A')
    val a1 = new Node[Character](null, A, 'a')
    val a2 = new Node[Character](null, null, 'a')
    val a3 = new Node[Character](a2, a1, 'a')
    val a4 = new Node[Character](null, null, 'a')
    val a5 = new Node[Character](a3, a4, 'a')
    countUnival(a5) shouldBe 3
  }

  it should "lock a binary tree" in {
    val six = new LockableBinTree[Integer](null, null, 6)
    val four = new LockableBinTree[Integer](six, null, 4)
    six.parent = four
    val five = new LockableBinTree[Integer](null, null, 5)
    val two = new LockableBinTree[Integer](four, five, 2)
    four.parent = two
    five.parent = two
    val three = new LockableBinTree[Integer](null, null, 3)
    val one = new LockableBinTree[Integer](two, three, 1)
    two.parent = one
    three.parent = one

    one.isLocked shouldBe false
    one.lock() shouldBe one.isLocked
    one.lock() shouldBe false
  }

  it should "unlock a binary tree" in {
    val six = new LockableBinTree[Integer](null, null, 6)
    val four = new LockableBinTree[Integer](six, null, 4)
    six.parent = four
    val five = new LockableBinTree[Integer](null, null, 5)
    val two = new LockableBinTree[Integer](four, five, 2)
    four.parent = two
    five.parent = two
    val three = new LockableBinTree[Integer](null, null, 3)
    val one = new LockableBinTree[Integer](two, three, 1)
    two.parent = one
    three.parent = one

    one.isLocked shouldBe false
    one.unlock() shouldBe !one.isLocked
    one.lock() shouldBe one.isLocked
    one.unlock() shouldBe false
  }

  it should "find the second largest node" in {
    val five = new Node[Integer](null, null, 5)
    val ten = new Node[Integer](five, null, 5)
    var sndLargest = secondLargest(ten)
    sndLargest should not be null
    sndLargest shouldBe 5

    val thirty = new Node[Integer](null, null, 30)
    val twenty = new Node[Integer](null, thirty, 20)
    ten.right = twenty

    sndLargest = secondLargest(ten)
    sndLargest should not be null
    sndLargest shouldBe 20
  }

  it should "iterate a BST" in {
    val nine = new Node[Integer](null, null, 9)
    val twenty = new Node[Integer](null, null, 20)
    val fifteen = new Node[Integer](nine, twenty, 15)
    val three = new Node[Integer](null, null, 3)
    val seven = new Node[Integer](three, fifteen, 7)

    new BSTIterator(seven).toList should contain theSameElementsInOrderAs Seq(3, 7, 9, 15, 20)
  }

  it should "reconstruct a binary tree given the pre-order and in-order traversals" in {
    val in1 = IndexedSeq[Character]('d', 'b', 'e', 'a', 'f', 'c', 'g')
    val a: Node[Character] = reconstruct1(IndexedSeq('a', 'b', 'd', 'e', 'c', 'f', 'g'), in1)

    BinTrees.morrisInorder[Character](a).asScala should contain theSameElementsInOrderAs in1

    val in2 = IndexedSeq[Integer](10, 30, 40, 50, 60, 70, 90)
    val b: Node[Integer] = reconstruct1(IndexedSeq(50, 30, 10, 40, 70, 60, 90), in2)

    BinTrees.morrisInorder[Integer](b).asScala should contain theSameElementsInOrderAs in2

    val in3 = IndexedSeq[Integer](1, 2)
    val c: Node[Integer] = reconstruct1(IndexedSeq(1, 2), in3)

    BinTrees.morrisInorder[Integer](c).asScala should contain theSameElementsInOrderAs in3

    val in4 = IndexedSeq[Integer](1, 2, 3, 4)
    val d: Node[Integer] = reconstruct1(IndexedSeq(3, 1, 2, 4), in4)

    BinTrees.morrisInorder[Integer](d).asScala should contain theSameElementsInOrderAs in4
  }

  it should "evaluate a binary tree" in {
    val three = new Node[Character](null, null, '3')
    val two = new Node[Character](null, null, '2')
    val plus1 = new Node[Character](three, two, '+')
    val four = new Node[Character](null, null, '4')
    val five = new Node[Character](null, null, '5')
    val plus2 = new Node[Character](four, five, '+')
    val multiply = new Node[Character](plus1, plus2, '*')

    eval(multiply) shouldBe (45d +- 0.001d)
  }

  it should "get tree level with minimum sum" in {
    val five = new Node[Integer](null, null, 5)
    val four = new Node[Integer](null, null, 4)
    val three = new Node[Integer](four, five, 3)
    val two = new Node[Integer](null, null, 2)
    val one = new Node[Integer](two, three, 1)

    minSumLevel(one) shouldBe 0
  }

  it should "find the floor and ceiling of a given integer" in {
    val two = new Node[Integer](null, null, 2)
    val six = new Node[Integer](null, null, 6)
    val four = new Node[Integer](two, six, 4)

    val nine = new Node[Integer](null, null, 9)
    val twelve = new Node[Integer](null, null, 12)
    val ten = new Node[Integer](nine, twelve, 10)

    val eight = new Node[Integer](four, ten, 8)

    val data = Table(
      ("i", "floor", "ceil"),
      (1, None, Some(2)),
      (3, Some(2), Some(4)),
      (9, Some(9), Some(9)),
      (7, Some(6), Some(8))
    )

    forAll(data) { (i, floor, ceil) =>
      floorAndCeiling(eight, i) shouldBe(floor, ceil)
    }
  }

  it should "return a deepest node" in {
    val d = new Node[Integer](null, null, 'd'.toInt)
    val b = new Node[Integer](d, null, 'b'.toInt)
    val c = new Node[Integer](null, null, 'c'.toInt)
    maxDepth(new Node[Integer](b, c, 'a'.toInt)) shouldBe d.getDatum
  }

  it should "invert a tree" in {
    val one = new Node[Integer](null, null, 1)
    val three = new Node[Integer](null, null, 3)
    val two = new Node[Integer](one, three, 2)
    val six = new Node[Integer](null, null, 6)
    val nine = new Node[Integer](null, null, 9)
    val seven = new Node[Integer](six, nine, 7)
    val four = new Node[Integer](two, seven, 4)

    val inverted = BinTrees.invertTree(four)
    BinTrees.morrisInorder[Integer](inverted).asScala should contain theSameElementsInOrderAs
      IndexedSeq[Integer](9, 7, 6, 4, 3, 2, 1)
  }

  it should "determine whether a tree is a valid binary search tree" in {
    isValidBST(BinTrees.buildBST[Integer](Array[Integer](2, 1, 3))) shouldBe true
    val six = new Node[Integer](null, null, 6)
    val twenty = new Node[Integer](null, null, 20)
    val fifteen = new Node[Integer](six, twenty, 15)
    val five = new Node[Integer](null, null, 5)
    val ten = new Node[Integer](five, fifteen, 10)

    isValidBST(ten) shouldBe false
  }

  it should "the size of the largest subtree that is a BST" in {
    val one = new Node[Integer](null, null, 1)
    val three = new Node[Integer](null, null, 3)
    val two = new Node[Integer](one, three, 2)
    val four = new Node[Integer](null, null, 4)
    val five = new Node[Integer](two, four, 5)

    largestBSTSize(five) shouldBe 3
    largestBSTSize(one) shouldBe 1
    largestBSTSize(two) shouldBe 3

    val ten = new Node[Integer](null, null, 10)
    val twenty = new Node[Integer](null, null, 20)
    val thirty = new Node[Integer](ten, twenty, 30)

    val sixtyFive = new Node[Integer](null, null, 65)
    val eighty = new Node[Integer](null, null, 80)
    val seventy = new Node[Integer](sixtyFive, eighty, 70)

    val fortyFive = new Node[Integer](null, null, 45)
    val sixty = new Node[Integer](fortyFive, seventy, 60)

    val fifty = new Node[Integer](thirty, sixty, 50)

    largestBSTSize(fifty) shouldBe 5
  }

  it should "find the maximum path sum" in {
    val data = Table(
      ("root", "sum"),
      (new Node[Integer](Array[Integer](-10, 9, 20, null, null, 15, 7)), 42),
      (new Node[Integer](Array[Integer](5, 4, 8, 11, null, 13, 4, 7, 2, null, null, null, 1)), 49),
      (new Node[Integer](Array[Integer](-3)), -3)
    )

    forAll(data) { (root, sum) =>
      maxPathSum(root) shouldBe sum
    }
  }

  it should "return all paths from the root to leaves" in {
    binaryTreePaths(new Node[Integer](Array[Integer](1, 2, 3, null, 5))) should contain theSameElementsAs Seq(
      Seq(1, 2, 5), Seq(1, 3)
    )
    binaryTreePaths(new Node[Integer](Array[Integer](1, 2, 3, null, null, 4, 5))) should contain theSameElementsAs Seq(
      Seq(1, 2), Seq(1, 3, 4), Seq(1, 3, 5)
    )
  }

  it should "check if one tree is a subtree of another" in {
    val data = Table(
      ("s", "t", "subtree"),
      (new Node[Integer](Array[Integer](3, 4, 5, 1, 2)), new Node[Integer](Array[Integer](4, 1, 2)), true),
      (new Node[Integer](Array[Integer](1, 1)), new Node[Integer](Array[Integer](1)), true),
      (
        new Node[Integer](Array[Integer](-9, null, 3, 2, 8, -1, null, 7, null, -4, 0, 6, null, -5)),
        new Node[Integer](Array[Integer](-1, 1515, 0, -5)), false
      ),
      (
        new Node[Integer](Array[Integer](3, 4, 5, 1, 2, null, null, 0)),
        new Node[Integer](Array[Integer](4, 1, 2)),
        false
      ),
      (
        new Node[Integer](Array[Integer](3, 4, 5, 1, 2)),
        new Node[Integer](Array[Integer](4, 1)),
        false
      ),
      (new Node[Integer](Array[Integer](12)), new Node[Integer](Array[Integer](2)), false)
    )

    forAll(data) { (s, t, subtree) =>
      isSubtree(s, t) shouldBe subtree
    }
  }

  it should "return the level with minimum sum" in {
    val root = new Node[Integer](Array[Integer](55, 50, -10, -25, 25, 1, -1, null, null, null, null, 10, 35))
    levelwithMinSum(root) shouldBe 2
  }

  it should "find if there exist two nodes whose sum equals k" in {
    twoSum(new Node[Integer](Array[Integer](10, 5, null, null, 15, 11, 15)), 20) shouldBe true
    twoSum(new Node[Integer](Array[Integer](5, 3, 6, 2, 4, 7)), 9) shouldBe true
    twoSum(new Node[Integer](Array[Integer](5, 3, 6, 2, 4, 7)), 28) shouldBe false
    twoSum(new Node[Integer](Array[Integer](2, 1, 3)), 4) shouldBe true
  }
}

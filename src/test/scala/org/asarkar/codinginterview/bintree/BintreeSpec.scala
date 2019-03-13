package org.asarkar.codinginterview.bintree

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.collection.JavaConverters._

class BintreeSpec extends FlatSpec {
  "bintree" should "construct a binary tree from the given array" in {
    val root = new Node[Integer](Array[Integer](6, 5, 4, 3, 2, 1))
  }

  it should "do a Morris inorder traversal" in {
    val xs = Array[Integer](6, 5, 4, 3, 2, 1)
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

  // c.f. src/test/resources/unival.jpg
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
}

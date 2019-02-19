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
}

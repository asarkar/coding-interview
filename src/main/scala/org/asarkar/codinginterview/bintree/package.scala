package org.asarkar.codinginterview

import scala.annotation.tailrec
import scala.util.matching.Regex

package object bintree {
  /*
   * Lowest Common Ancestor
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/bintree/package.scala
   */

  /*
   * Is BST?
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/bst/package.scala
   */

  /*
   * Given the root to a binary tree, implement serialize(root), which serializes the tree into a string, and
   * deserialize(s), which deserializes the string back into the tree.
   *
   * ANSWER: We will serialize as a full binary tree preorder traversal, using # to indicate null, and space
   * to separate nodes. The number of null nodes = number of nodes + 1, so we are using 2n space.
   * For example, Node(null, Node(null, null, 2), 1) will be serialized as '1 # 2 # #'
   */
  def serialize[T](root: Node[T]): String = {
    def serialize(node: Node[T], buffer: StringBuilder): Unit = {
      if (node == null) buffer.append("# ")
      else {
        buffer.append(s"${node.getDatum} ")
        serialize(node.getLeft, buffer)
        serialize(node.getRight, buffer)
      }
    }

    val buffer = new StringBuilder()
    serialize(root, buffer)
    buffer.delete(buffer.size - 1, buffer.size).toString()
  }

  // null is a subtype of AnyRef, not AnyVal. Scala Int is a subtype of AnyVal, so we can't assign null to an
  // Int type; have to use Java Integer type
  def deserialize(str: String): Node[Integer] = {
    def deserialize(nodes: Regex.MatchIterator): Node[Integer] = {
      if (nodes.hasNext) {
        val node = nodes.next()
        if (node == "#") null else new Node[Integer](deserialize(nodes), deserialize(nodes), node.toInt)
      } else null
    }

    deserialize("\\S+".r.findAllIn(str))
  }

  /*
   * A unival tree (which stands for "universal value") is a tree where all nodes under it have the same value.
   * Given the root to a binary tree, count the number of unival subtrees.
   */
  def countUnival[T](root: Node[T]): Int = {
    def loop(node: Node[T]): (Int, Boolean) = {
      Seq(node.getLeft, node.getRight)
        .filter(_ != null) match {
        case xs =>
          val (subCount, subUnival) = xs.map(loop).foldLeft((0, true)) { case ((x, b1), (y, b2)) =>
            (x + y, b1 && b2)
          }
          val unival = subUnival && xs.forall(_.getDatum == node.getDatum)
          val count = (if (unival) 1 else 0) + subCount
          (count, unival)
      }
    }

    loop(root)._1
  }

  /*
   * Given the root to a binary search tree, find the second largest node in the tree.
   *
   * ANSWER: This question is basically asking to find the inorder predecessor of the maximum node. When there's no
   * right subtree, the maximum node is the root node (recursively). Its inorder predecessor is its left child.
   * When there's a right subtree, the maximum node is the rightmost element. Its inorder predecessor is its parent.
   */
  def secondLargest(root: Node[Integer]): Integer = {
    @tailrec
    def loop(node: Node[Integer], parent: Node[Integer]): Integer = {
      if (node == null || (node.getLeft == null && node.getRight == null))
        if (node != null && parent.getDatum > node.getDatum) node.getDatum
        else parent.getDatum
      else if (node.getRight == null) loop(node.getLeft, node)
      else loop(node.getRight, node)
    }

    loop(root, null)
  }

  /*
   * Given pre-order and in-order traversals of a binary tree, write a function to reconstruct the tree.
   *
   * For example, given the following preorder traversal:
   * [a, b, d, e, c, f, g]
   * And the following inorder traversal:
   * [d, b, e, a, f, c, g]
   *
   * You should return the following tree:
   *       a
   *   +-------+
   *   b       c
   * +---+   +---+
   * d   e   f   g
   *
   * ANSWER: Notice that the first element of the preorder is the root. If we find the root in the inorder, all elements
   * on its left are in the left subtree, and all elements on its right are in the right subtree.
   * We recursively build the left and right subtrees, keeping track of the roots. The index of the root of the left
   * subtree is one more than the index of the current root in the preorder (by definition). The root of the right
   * subtree is the number of elements in the left subtree, plus one (for the root); its index is thus equal to the
   * size of the left subtree.
   */

  def reconstruct1[T](pre: IndexedSeq[T], in: IndexedSeq[T]): Node[T] = {
    val inorderIndexMap = in.zipWithIndex.toMap

    // builds the tree contained between in[left..right]. returns the root and the size of the newly built tree
    def buildTree(left: Int, right: Int, preIdx: Int): (Node[T], Int) = {
      if (left == right) (new Node[T](null, null, in(left)), preIdx + 1)
      else {
        val mid = inorderIndexMap(pre(preIdx))
        val (l, x) = buildTree(left, mid - 1, preIdx + 1)
        val (r, y) = buildTree(mid + 1, right, x)
        (new Node[T](l, r, in(mid)), y)
      }
    }

    val (root, size) = buildTree(0, in.size - 1, 0)
    assert(
      size == pre.size,
      s"Size of the reconstructed tree: $size doesn't match the given number of vertices: ${pre.size}"
    )
    root
  }

  /*
   * Suppose an arithmetic expression is given as a binary tree. Each leaf is an integer and each internal node is one
   * of '+', '-', '∗', or '/'.
   * Given the root to such a tree, write a function to evaluate it.
   *
   * For example, given the following tree:
   *
   *      *
   *   +-----+
   *   |     |
   * +-+-+ +-+-+
   * 3   2 4   5
   *
   * You should return 45, as it is (3 + 2) * (4 + 5).
   */
  def eval(root: Node[Character]): Double = {
    if (root == null) 0
    else if (Character.isDigit(root.getDatum)) root.getDatum.toChar.asDigit
    else {
      val left = eval(root.getLeft)
      val right = eval(root.getRight)

      root.getDatum.toChar match {
        case '+' => left + right
        case '-' => left - right
        case '*' => left * right
        case '/' if right != 0d => (1d * left) / right
      }
    }
  }
}

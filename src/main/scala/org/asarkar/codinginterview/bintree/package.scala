package org.asarkar.codinginterview

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
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
   * Examples: See unival.png. The various unival trees are shown inside light blue ellipses.
   *
   * ANSWER: A node is a unival tree is both of its children are unival trees, and if its value is equal to the values
   * of its children (if any or both of the children are absent, they count as empty unival trees). The following
   * algorithm follows straight from the above observation.
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
      if (left > right || !pre.isDefinedAt(preIdx)) (null, -1)
      else {
        val rootIdx = inorderIndexMap(pre(preIdx))
        val (l, lIdx) = buildTree(left, rootIdx - 1, preIdx + 1)
        val (r, rIdx) = buildTree(rootIdx + 1, right, math.max(preIdx, lIdx) + 1)
        (new Node[T](l, r, pre(preIdx)), Seq(preIdx, lIdx, rIdx).max)
      }
    }

    buildTree(0, in.size - 1, 0)._1
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

  /*
   * Given a binary tree, return the level of the tree that has the minimum sum. The level of a node is defined as the
   * number of connections required to get to the root, with the root having level zero.
   */
  def minSumLevel(root: Node[Integer]): Int = {
    def minSum(level: Int, nodes: Seq[Node[Integer]]): Int = {
      if (nodes.isEmpty) 0
      else {
        val (ch, s) = nodes
          .foldLeft((ListBuffer.empty[Node[Integer]], 0)) { case ((children, sum), n) =>
            (children ++= Seq(n.getLeft, n.getRight).filterNot(_ == null), sum + n.getDatum)
          }
        math.min(s, minSum(level + 1, ch))
      }
    }

    minSum(0, Seq(root))
  }

  /*
   * Given a binary search tree, find the floor and ceiling of a given integer. The floor is the highest element in the
   * tree less than or equal to the integer, while the ceiling is the lowest element in the tree greater than or equal
   * to the integer.
   * If either of the values does not exist, return null.
   */
  def floorAndCeiling(root: Node[Integer], v: Int): (Option[Int], Option[Int]) = {
    @tailrec
    def loop(node: Node[Integer], floor: Option[Int], ceil: Option[Int]): (Option[Int], Option[Int]) = {
      if (node == null) (floor, ceil)
      else if (node.getDatum == v) (Some(node.getDatum), Some(node.getDatum))
      else if (node.getDatum < v) loop(node.getRight, Some(node.getDatum), ceil)
      else loop(node.getLeft, floor, Some(node.getDatum))
    }

    loop(root, None, None)
  }

  /*
   * Given the root of a binary tree, return a deepest node. For example, in the following tree, return d.
   */
  def maxDepth(root: Node[Integer]): Int = {
    def loop(node: Node[Integer], currD: Int, maxD: Int): (Int, Int) = {
      if (node == null) (currD - 1, maxD)
      else {
        val l = loop(node.getLeft, currD + 1, node.getDatum)
        val r = loop(node.getRight, currD + 1, node.getDatum)
        if (l._1 > r._1) l else r
      }
    }

    loop(root, 1, -1)._2
  }

  /*
   * Determine whether a tree is a valid binary search tree.
   *
   * A binary search tree is a tree with two children, left and right, and satisfies the constraint that the key in the
   * left child must be less than or equal to the root and the key in the right child must be greater than or equal to
   * the root.
   *
   * ANSWER: It is not sufficient to check that the values at the left and right nodes are less than, and greater than,
   * the value at the node, respectively. Every value in the left subtree must be less than the value at the node.
   * Similarly, every value at the right subtree must be greater than the value at the node.
   */
  def isValidBST(root: Node[Integer]): Boolean = {
    def isValid(min: Double, max: Double, node: Node[Integer]): Boolean = {
      if (node == null) true
      else if (node.getDatum <= min || node.getDatum >= max) false
      else isValid(min, node.getDatum.toDouble, node.getLeft) && isValid(node.getDatum.toDouble, max, node.getRight)
    }

    isValid(Double.NegativeInfinity, Double.PositiveInfinity, root)
  }

  /*
   * Given a tree, return the size of the largest tree/subtree that is a BST.
   *
   * Example:
   *
   *           5
   *      +--------+
   *     2         4
   * +--------+
   * 1        3
   *
   * Output: 3 (subtree rooted at 2).
   *
   * Another example:
   *
   *             50
   *    +-----------------+
   *    |                 |
   *    +                 +
   *   30                 60
   * +--------+      +-----------+
   * 10       20     45          70
   *                         +--------+
   *                         65       80
   *
   * Output: 5 (subtree rooted at 60).
   *
   * ANSWER: We solve this problem in a bottom-up manner. For each node, we pass the following values up to its parent:
   * (isBST, sizeOfLargestBSTFoundSoFar, maxValueInLeftSubtree, minValueInRightSubtree).
   *
   * If the tree rooted at the current node is a BST, the size of the largest BST found so far is the size of the
   * current tree, which, in turn, is the size of the left and right subtrees plus 1. If it's not a BST, the size of
   * the largest BST found so far is passed up unchanged.
   *
   * The min and max values are used to test if the current tree is a BST. If the current tree is not a BST, none of
   * the trees higher up can be BST either, so the min and the max values are set to the largest and the smallest
   * possible, respectively.
   *
   * Time complexity: O(n), since each node is visited exactly once.
   */
  def largestBSTSize(root: Node[Integer]): Int = {
    def loop(node: Node[Integer]): (Boolean, Int, Int, Int) = {
      if (node == null) (true, 0, Int.MinValue, Int.MaxValue)
      else {
        val l = loop(node.getLeft)
        val r = loop(node.getRight)

        val bst = l._1 && r._1 && node.getDatum >= l._3 && node.getDatum <= r._4
        val size = if (bst) 1 + l._2 + r._2 else math.max(l._2, r._2)
        val min: Int = if (bst) node.getDatum else Int.MaxValue
        val max: Int = if (bst) node.getDatum else Int.MinValue

        (bst, size, min, max)
      }
    }

    loop(root)._2
  }

  /*
   * Given a binary tree of integers, find the maximum path sum between two nodes. The path must go through at least
   * one node, and does not need to go through the root.
   *
   * ANSWER: Although the question doesn't clearly say, by path, it means a simple linear path consisting of contiguous
   * nodes. In other words, we should be able to trace this path on paper from one end to the other without
   * backtracking. This implies there should be no splitting at any point of the path. Since we are working with a
   * binary tree, the maximum path will have a root, and include none, one, or both of the subtrees.
   *
   * At each node, there are three possibilities:
   * 1. This node is on the maximum path, and is NOT the root of the tree that contains the maximum path. Since we are
   *    looking for contiguous nodes only, the root must be higher up. The maximum path consists of the node, and
   *    possibly one of its subtrees, but not both (since this node isn't the root).
   * 2. This node is on the maximum path, and is the root of the tree that contains the maximum path. The maximum path
   *    consists of the node, and possibly both of its subtrees (since this node is the root).
   * 3. This node is not on the maximum path.
   *
   * The answer is the maximum of the values from all case 2.
   *
   * Time complexity: O(n), since we visit all nodes once.
   */
  def maxPathSum(root: Node[Integer]): Int = {
    def loop(node: Node[Integer], max: Int): (Int, Int) = {
      if (node == null) (0, max)
      else {
        val l = loop(node.getLeft, max)
        val r = loop(node.getRight, max)

        val pathMax = math.max(node.getDatum, node.getDatum + math.max(l._1, r._1))
        val treeMax = Seq(l._2, r._2, pathMax, node.getDatum + l._1 + r._1).max

        (pathMax, treeMax)
      }
    }

    loop(root, if (root == null) 0 else root.getDatum)._2
  }

  /*
   * Given a binary tree, return all paths from the root to leaves.
   * For example, given the tree:
   *    1
   * +-----+
   * 2     3
   *    +-----+
   *    4     5
   *
   * Return [[1, 2], [1, 3, 4], [1, 3, 5]].
   */
  def binaryTreePaths(root: Node[Integer]): Seq[Seq[Integer]] = {
    def loop(node: Node[Integer], partial: Seq[Integer]): Seq[Seq[Integer]] = {
      if (node == null) Seq.empty
      else {
        val xs = partial :+ node.getDatum
        if (node.getLeft == null && node.getRight == null) Seq(xs)
        else loop(node.getLeft, xs) ++ loop(node.getRight, xs)
      }
    }

    loop(root, Seq.empty)
  }

  /*
   * Given two non-empty binary trees s and t, check whether tree t has exactly the same structure and node values
   * with a subtree of s. A subtree of s is a tree consists of a node in s and all of this node's descendants.
   * The tree s could also be considered as a subtree of itself.
   *
   * ANSWER: We do pre-order traversal of the trees and check if one path is contained in the other. However,
   * we need to differentiate between left and right null trees. Why? Consider the following trees:
   *        3
   *    +------+
   *    4      5
   * +-----+
   * 1     2
   *
   * Pre-order: 3 4 1 2 5
   *
   * and
   *    4
   * +--+
   * 1
   * Pre-order: 4 1
   *
   * The latter is not a subtree of the former, but its pre-order path is contained in the former. To prevent this,
   * we indicate left null as "lnull", and right null as "rnull". Now the pre-order paths are
   * "3 4 1 lnull rnull 2 lnull rnull 5 lnull rnull" and " 4 1 lnull rnull rnull" respectively, and the latter is not
   * contained in the former.
   *
   * We aren't done yet. Consider the trees s = 12 and t = 2. The pre-order paths are "12" and "2", and gives
   * a false result that t is a subtree of s. To prevent this, we prepend '#' to the value of every non-null node.
   * Now the pre-order paths are "#12" and "#2" respectively, and the latter is not contained in the former.
   *
   * Time complexity: Assuming appending to StringBuilder takes constant time, pre-order traversal takes linear time.
   * contains check may take linear time if all characters of larger have to be checked. Thus, overall time complexity:
   * O(n), where n is the size of the larger tree.
   */
  def isSubtree(s: Node[Integer], t: Node[Integer]): Boolean = {
    def preOrder(node: Node[Integer], sb: StringBuilder): StringBuilder = {
      sb.append('#').append(node.getDatum)

      if (node.getLeft == null) sb.append("lnull")
      else preOrder(node.getLeft, sb)

      if (node.getRight == null) sb.append("rnull")
      else preOrder(node.getRight, sb)

      sb
    }

    val sb1 = preOrder(s, new StringBuilder())
    val sb2 = preOrder(t, new StringBuilder())
    val (smaller, larger) = if (sb1.size < sb2.size) (sb1, sb2) else (sb2, sb1)

    larger.toString().contains(smaller.toString())
  }

  /*
   * Given a binary tree, return the level of the tree with minimum sum.
   * Example: See level-with-min-sum.png
   *
   * ANSWER: We maintain a running sum for a level, and a queue to keep track of the yet unprocessed nodes. At each
   * iteration, we pop a node from the queue, and add its children (if any) to the queue. When the top node in the
   * queue has a level greater than the one popped, we have processed all nodes in the current level. We then compare
   * the running sum with the minimum sum, and return the level corresponding to the smaller value.
   *
   * Time complexity: O(n).
   */
  def levelwithMinSum(root: Node[Integer]): Int = {
    val queue = ListBuffer((root, 0))
    Iterator.iterate((0, Int.MaxValue, 0)) { case (sum, min, minLevel) =>
      if (queue.isEmpty) (sum, min, minLevel)
      else {
        val (node, level) = queue.remove(0)
        if (node.getLeft != null) queue.append((node.getLeft, level + 1))
        if (node.getRight != null) queue.append((node.getRight, level + 1))

        if (queue.headOption.map(_._2).getOrElse(-1) > level) {
          if (sum + node.getDatum < min) (0, sum + node.getDatum, level)
          else (0, min, minLevel)
        } else (sum + node.getDatum, min, minLevel)
      }
    }
      .dropWhile(_ => queue.nonEmpty)
      .take(1)
      .map(_._3)
      .next()
  }

  /*
   * Given a Binary Search Tree and a target number, return true if there exist two elements in the BST such that their
   * sum is equal to the given target.
   */
  def twoSum(root: Node[Integer], k: Int): Boolean = {
    def loop(node: Node[Integer], values: collection.mutable.Set[Int]): Boolean = {
      if (node == null) false
      else values.contains(k - node.getDatum) ||
        loop(node.getLeft, values += node.getDatum) ||
        loop(node.getRight, values)
    }

    loop(root, collection.mutable.Set.empty[Int])
  }
}

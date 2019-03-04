package org.asarkar.codinginterview

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
}

package org.asarkar.codinginterview.bintree

import scala.collection.mutable.ListBuffer

/*
 * Implement an iterator over a binary search tree (BST). Your iterator will be initialized with the root node of a
 * BST.
 * Calling next() will return the next smallest number in the BST.
 */
class BSTIterator(val root: Node[Integer]) extends collection.Iterator[Integer] {
  private val stack = ListBuffer.empty[Node[Integer]]

  push(root)

  override def hasNext: Boolean = stack.nonEmpty

  override def next(): Integer = {
    val e = stack.remove(0)
    push(e.right)
    e.datum
  }

  private def push(node: Node[Integer]): Unit = {
    if (node != null)
      Iterator.iterate(node)(_.left)
        .takeWhile(_ != null)
        .foreach(_ +=: stack)
  }
}

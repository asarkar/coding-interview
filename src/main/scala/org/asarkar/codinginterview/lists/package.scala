package org.asarkar.codinginterview

package object lists {
  /*
   * K-th element of a Linked List
   */
  def kthNode[T](root: LinkedList[T], k: Int): Option[T] = {
    Iterator.iterate((root, 1)) { case (node, count) =>
      (node.next, count + 1)
    }
      .dropWhile(x => x._1.nonEmpty && x._2 < k)
      .take(1)
      .map(x => Option(x._1))
      .next
      .filter(_.nonEmpty)
      .map(_.datum)
  }

  /*
   * K-th element from the end of a Linked List
   *
   * ANSWER: Two pointers, fast and slow. Move fast k steps. Distance between it and the slow pointer is k. Now move
   * both one step at a time. When fast reaches the end, slow is still distance k behind, which is where we want it to
   * be.
   */
  def kthNodeFromTheEnd[T](root: LinkedList[T], k: Int): Option[T] = {
    val fast = Iterator.iterate((root, 1)) { case (node, count) =>
      (node.next, count + 1)
    }
      .dropWhile(x => x._1.nonEmpty && x._2 < k)
      .take(1)
      .map(_._1)
      .next

    Iterator.iterate((fast, root)) { case (x, y) =>
      (x.next, y.next)
    }
      .dropWhile(x => x._1.nonEmpty && x._1.next.nonEmpty)
      .take(1)
      .filter(x => x._1.nonEmpty && x._2.nonEmpty)
      .map(_._2)
      .toList match {
      case scala.collection.immutable.Nil => None
      case xs => xs.headOption.map(_.datum)
    }
  }

  /*
   * Reverse singly linked list
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/lists/package.scala
   */

  /*
   * Detect cycle
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/lists/package.scala
   */

  /*
   * Sort a linked list
   *
   * ANSWER: We use merge sort.
   * 1. Finding the pivot takes linear time.
   * 2. Merge takes linear time.
   * 3. We halve the problem size each time.
   *
   * Overall time complexity is O(n log(n)).
   */
  def mergeSort[T](xs: LinkedList[T])(implicit ord: Ordering[T]): LinkedList[T] = {
    if (xs.isEmpty || xs.next.isEmpty) xs
    else {
      val (left, right) = split(xs)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def merge[T](left: LinkedList[T], right: LinkedList[T])(implicit ord: Ordering[T]): LinkedList[T] = {
    Seq(left, right)
      .filter(_.nonEmpty) match {
      case Seq(l, r) =>
        if (ord.lt(l.datum, r.datum)) LinkedList(l.datum, merge(l.next, r))
        else LinkedList(r.datum, merge(r.next, l))
      case Seq(xs) => xs
      case _ => LinkedList.empty[T]
    }
  }

  // find the middle node by running two pointers, slow and fast. when fast is at the last node, slow is at the middle
  // one
  def split[T](xs: LinkedList[T]): (LinkedList[T], LinkedList[T]) = {
    Iterator.iterate((Seq.empty[T], xs, xs)) { case (acc, slow, fast) =>
      if (fast.next.isEmpty) (acc, slow, LinkedList.empty[T])
      else (acc :+ slow.datum, slow.next, fast.next.next)
    }
      .dropWhile(_._3.nonEmpty)
      .take(1)
      .map(x => (LinkedList(x._1: _*), x._2))
      .next()
  }
}

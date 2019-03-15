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

  /*
   * cons(a, b) constructs a pair, and car(pair) and cdr(pair) returns the first and last element of that pair.
   * For example, car(cons(3, 4)) returns 3, and cdr(cons(3, 4)) returns 4.
   * Given this implementation of cons:
   * def cons(a, b):
   *   def pair(f):
   *     return f(a, b)
   * return pair
   *
   * Implement car and cdr.
   *
   * ANSWER: This question is about purely functional programming. The Python implementation of cons is a little hard
   * to follow due to the absence of type information, so let me break it down.
   *
   * cons is a higher-order function that accepts two values, and...get this...returns a function that accepts another
   * function that knows how to operate on those values! The return type of cons is an Int, which is the result of
   * applying the third function on the given values.
   * car is a higher-order function that takes the output of cons, and calls it with an anonymous function that
   * extracts the first element of the given tuple.
   * cdr is a higher-order function that takes the output of cons, and calls it with an anonymous function that
   * extracts the second element of the given tuple.
   *
   * Let that sink in for a moment, and the following implementation will be self-evident.
   */

  def cons(a: Int, b: Int): ((Int, Int) => Int) => Int = {
    def pair(f: (Int, Int) => Int): Int = f(a, b)

    pair
  }

  def car(f: ((Int, Int) => Int) => Int): Int = {
    f((a: Int, _: Int) => a)
  }

  def cdr(f: ((Int, Int) => Int) => Int): Int = {
    f((_: Int, b: Int) => b)
  }

  /*
   * An XOR linked list is a more memory efficient doubly linked list. Instead of each node holding next and prev
   * fields, it holds a field named both, which is an XOR of the next node and the previous node. Implement an XOR
   * linked list; it has an add(element) which adds the element to the end, and a get(index) which returns the node
   * at index.
   *
   * If using a language that has no pointers (such as Python), you can assume you have access to get_pointer and
   * dereference_pointer functions that converts between nodes and memory addresses.
   *
   * ANSWER: The idea is that storing the absolute address takes more space than storing the XOR of two absolute
   * addresses. To traverse the list in forward direction, we simply perform prev XOR both; to traverse the list
   * backwards, we do next XOR both. Wikipedia explains the math behind it pretty well.
   * https://en.wikipedia.org/wiki/XOR_linked_list
   *
   * The original article seems to be this: https://www.linuxjournal.com/article/6828
   *
   * Since no JVM language supports pointer manipulation, I'm not going to implement the XOR linked list.
   */

  /*
   * Given two singly linked lists that intersect at some point, find the intersecting node. The lists are non-cyclical.
   *
   * For example, given A = 3 -> 7 -> 8 -> 10 and B = 99 -> 1 -> 8 -> 10, return the node with value 8.
   * In this example, assume nodes with the same value are the exact same node objects.
   *
   * Do this in O(M + N) time (where M and N are the lengths of the lists) and constant space.
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/lists/package.scala
   */

  /*
   * Given a singly linked list and an integer k, remove the kth last element from the list. k is guaranteed to be
   * smaller than the length of the list.
   * The list is very long, so making more than one pass is prohibitively expensive.
   * Do this in constant space and in one pass.
   *
   * ANSWER: See "K-th element from the end of a Linked List" above.
   */
}

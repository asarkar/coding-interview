package org.asarkar.codinginterview.stacksnqueues

import scala.collection.mutable.ListBuffer

/*
 * Implement a queue using two stacks. Recall that a queue is a FIFO (first-in, first-out) data structure with the
 * following methods: enqueue, which inserts an element into the queue, and dequeue, which removes it.
 *
 * ANSWER: Whenever dequeue is called, if stack2 is empty, we transfer all the elements from stack1 to it. This
 * puts the oldest element on the top, which we then pop and return.
 * Note that there is no need of transference if stack2 is not empty.
 *
 * Time complexity: O(1) for enqueue, O(n) for dequeue in the worst case.
 * Space complexity: O(n), each element is only in one of the two stacks.
 */
class Queue[A] {
  private val stack1 = ListBuffer.empty[A]
  private val stack2 = ListBuffer.empty[A]

  def enqueue(a: A): Unit = {
    a +=: stack1
  }

  def dequeue(): A = {
    if (stack1.isEmpty && stack2.isEmpty)
      throw new NoSuchElementException("cannot dequeue from empty queue")
    else if (stack2.isEmpty)
      while (stack1.nonEmpty) stack1.remove(0) +=: stack2

    stack2.remove(0)
  }
}

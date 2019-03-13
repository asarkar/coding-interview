package org.asarkar.codinginterview

package object design {
  /*
   * You run an e-commerce website and want to record the last N order ids in a log. Implement a data structure to
   * accomplish this, with the following API:
   * record(order_id): adds the order_id to the log
   * get_last(i): gets the ith last element from the log. i is guaranteed to be smaller than or equal to N.
   *
   * You should be as efficient with time and space as possible.
   *
   * ANSWER: We can do this using a Ring Buffer of size N. We implement the Ring Buffer using an array. There are
   * two ways of finding the position an element should be inserted to. If we are keeping count of the number of
   * order seen so far, say k, the index to insert the (k + 1)-th element is (k % N).
   * If we are not keeping track of the number of orders, we can maintain a pointer that gets increments each time
   * we insert a new element. When it reaches the end of the array, it is reset to zero.
   */
}
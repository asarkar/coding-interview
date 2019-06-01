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

  /*
   * Implement a file syncing algorithm for two computers over a low-bandwidth network. What if we know the files in
   * the two computers are mostly the same?
   *
   * ANSWER: We can build Merkle trees for the two computers, and then transmit only those files that have changed.
   * In the worst case, every file could have changed, in which case, we need to compare every node in the two trees.
   * That would take linear time in terms of the number of nodes in the smaller tree.
   * To compute the hash, we could do something similar to what Git does: use the content for files, and name and
   * hash of children by lexicographical ordering of their names for directories.
   * See https://blog.asarkar.org/coding-interview-curated/#hashing for some interesting references on Merkle trees.
   */

  /*
   * Given a function f, and N return a debounced f of N milliseconds.
   *
   * That is, as long as the debounced f continues to be invoked, f itself will not be called for N milliseconds.
   *
   * ANSWER: The question isn't clear. Seems like if f hasn't been executed, calling debounce should reset the timer.
   * What should happen if debounce is called after f has been executed? Also, when debounce is called on the main
   * thread, should it block, or schedule execution on a separate thread? Is debounce supposed to be thread-safe?
   *
   * c.f. https://stackoverflow.com/a/20978973/839733
   */
}

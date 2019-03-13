package org.asarkar.codinginterview

package object heaps {
  /*
   * K closest points to the origin (on a 2D plane)
   *
   * See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/heaps/package.scala
   */

  /*
   * You are given N parts of various sizes to assemble, and you can combine two smaller parts to make a bigger one.
   * The time taken to combine two parts is the sum of their sizes. How would you do the assembly such that the total
   * assembly time is minimized?
   *
   * Example: Given parts of sizes [3, 2, 1, 4], the optimal assembly is as follows:
   * Combine 1 and 2. Parts: [3, 3, 4]
   * Combine 3 and 3. Parts: [6, 4]
   * Combine 6 and 4. Parts: [10]
   * Total time taken: 3 + 6 + 10 = 19
   *
   * ANSWER: Insert the parts in a min heap. Extract the top two, combine, and put back in the heap.
   */
}

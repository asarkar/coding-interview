package org.asarkar.codinginterview

package object hashtables {
  /*
   * First recurring character
   *
   * ANSWER: Use a hashtable.
   */

  /*
   * 2-sum
   *
   * ANSWER: We make the following assumptions:
   * 1. Input array contains only distinct elements
   * 2. Order doesn't matter, i.e. (1, 3) = (3, 1)
   * 3. An element can't be paired with itself, i.e. (1, 1) is not a valid answer for k = 2.
   *
   * Time complexity: O(n), space complexity: O(n)
   */
  def twoSum(xs: IndexedSeq[Int], k: Int): Seq[(Int, Int)] = {
    val ys = xs.toSet

    xs
      .foldLeft(collection.mutable.Set.empty[(Int, Int)]) { case (acc, x) =>
        val y = if (x < k - x) (x, k - x) else (k - x, x)
        if (ys.contains(k - x) && x != k - x && !acc.contains(y)) acc += y

        acc
      }
      .toSeq
  }

  /*
   * 3-sum
   *
   * ANSWER: Similar to 2-sum, for each pair of elements (x, y), we check for the presence of k - (x + y).
   *
   * Time complexity: O(n²), space complexity: O(n)
   */
  def threeSum(xs: IndexedSeq[Int], k: Int): Seq[(Int, Int, Int)] = {
    val ys = xs.toSet

    xs
      .indices
      .foldLeft(collection.mutable.Set.empty[(Int, Int, Int)]) { case (acc, i) =>
        (i + 1 until xs.size)
          .foreach { j =>
            val x = xs(i) + xs(j)
            val zs = Seq(xs(i), xs(j), k - x).distinct.sorted
            if (zs.size == 3 && ys.contains(k - x) && !acc.contains((zs.head, zs.tail.head, zs.last))) {
              acc += ((zs.head, zs.tail.head, zs.last))
            }
          }
        acc
      }
      .toSeq
  }
}

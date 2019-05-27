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

  /*
   * There is a brick wall in front of you. The wall is rectangular and has several rows of bricks. The bricks have the
   * same height but different width. You want to draw a vertical line from the top to the bottom and cross the least
   * bricks.
   *
   * The brick wall is represented by a list of rows. Each row is a list of integers representing the width of each
   * brick in this row from left to right. If your line go through the edge of a brick, then the brick is not
   * considered as crossed. You need to find out how to draw the line to cross the least bricks and return the number
   * of crossed bricks.
   *
   * You cannot draw a line just along one of the two vertical edges of the wall, in which case the line will obviously
   * cross no bricks.
   *
   * Example:
   * Input:
   * [[1,2,2,1],
   * [3,1,2],
   * [1,3,2],
   * [2,4],
   * [3,1,2],
   * [1,3,1,1]]
   *
   * Output: 2
   *
   * ANSWER: We go ver each row and take a count of how many bricks end after a running width (except for the last
   * brick which is not allowed). We store this count in a hash map. For the example above, the map would contain
   * 1 -> 1, 2 -> 1, 5 -> 1 after processing the first row. In the end, the entry with the maximum value represents
   * the vertical line. For the given example, it would be 4 -> 4. Thus, we know to take the cut in the middle of the
   * 3rd brick in the first row, and not at either ends, which correspond to widths 3 and 5, respectively.
   *
   * Time complexity: We look at every brick except the last ones, so for a m x n wall, time complexity is O(mn). If
   * every accumulated width is unique, we would need O(mn) space too for the map.
   */
  def leastBricks(wall: IndexedSeq[IndexedSeq[Int]]): Int = {
    val accumulatedWidthToNumBricksEnding = collection.mutable.Map.empty[Int, Int]
    val maxNumBricksEnding = wall
      .foldLeft(-1) { (maxNumBricksEnding, row) =>
        row.dropRight(1).foldLeft((0, maxNumBricksEnding)) { case ((sum, max), width) =>
          val s = sum + width
          val numBricks = accumulatedWidthToNumBricksEnding.getOrElse(s, 0) + 1
          accumulatedWidthToNumBricksEnding.update(s, numBricks)
          (s, math.max(max, numBricks))
        }
          ._2
      }
    wall.size - maxNumBricksEnding
  }

  /*
   * Given an unsorted array of integers, find the length of the longest consecutive elements sequence.
   *
   * For example, given [100, 4, 200, 1, 3, 2], the longest consecutive element sequence is [1, 2, 3, 4].
   * Return its length: 4.
   *
   * Your algorithm should run in O(n) complexity.
   *
   * ANSWER: One way to solve this problem is by sorting the array, and then going over the sorted array keeping
   * track of the longest sequence. That takes O(n log n) time.
   *
   * A cleverer solution is to convert the array into a set, find the beginning of consecutive elements sequence, and
   * keep track of the longest one. We find the beginning of a sequence by checking if the current number - 1 is
   * present in the set.
   * This solution checks each element only once, thus time complexity is O(n).
   */
  def longestConsecutive(xs: Seq[Int]): Int = {
    val ys = xs.toSet

    ys
      .filterNot(i => ys.contains(i - 1))
      .map(Stream.from(_)
        .takeWhile(ys.contains)
        .size)
      .max
  }
}

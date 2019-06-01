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

  /*
   * Given a list of integers and a number K, return which contiguous elements of the list sum to K.
   *
   * For example, if the list is [1, 2, 3, 4, 5] and K is 9, then it should return [2, 3, 4], since 2 + 3 + 4 = 9.
   *
   * ANSWER: We slightly modify the given question to match LeetCode; instead of returning the actual subrrays that sum
   * to K, we return the number of such subarrays.
   *
   * One obvious solution is find all subarrays starting at i, for i in [0, n - 1], and check theirs sums. Each
   * subarray sum is just the current element plus the previous subarray sum, so by keeping a running sum for each i,
   * we can calculate each sum in O(1) time. Since for each i, we are calculating the sum of each subarray starting at
   * i, the time complexity is given by the number of subarrays. For i = 0, there are n subarrays, for i = 1, n - 1
   * subarrays, so on and so forth. Total number of subarrays = ∑(n - i), i in [0, n - 1]. This sum evaluates to
   * n² - n(n - 1)/2 = (n² + n)/2, which is O(n²) time complexity.
   *
   * A cleverer solution makes use of the observation that every subarray can be expressed as the difference of a larger
   * and a smaller subarray. For example, [2, 3, 4] is the difference of [1, 2, 3, 4] and [1]. Thus, for each subarray
   * starting at 0 and ending at i, it contains all possible smaller subarrays starting at [0, i] and ending at [0, j],
   * i <= j < n. If any of the constituent subarrays sum to k, we can express that in terms of the difference of the sum
   * of the larger subarray, and the sum of a smaller subarray that ends before the start of the constituent subarray.
   *
   * For the given example, there are two subarrays that sum to 9; [2, 3, 4] and [4, 5]. Since [2, 3, 4] is suffix of
   * [1, 2, 3, 4], we check if sum([1, 2, 3, 4]) - sum([?, ?, ..., ?] = k, or sum([1, 2, 3, 4]) - k = sum([?, ?, ..., ?].
   * We have seen sum([1, 2, 3, 4]) - 9 once before, which is 1, for the subarray ending at i = 0 (subrarray [1]).
   * Thus, we increment the count by 1.
   * Since [4, 5] is suffix of [1, 2, 3, 4, 5], we check if we have seen sum([1, 2, 3, 4, 5]) - 9 before, which is 6, and
   * we have for the subarray ending at i = 2 (subrarray [1, 2, 3]). Thus, we increment the count by 1.
   * The final count is 2.
   *
   * The strategy, therefore, is to maintain a map of sum -> count, where sum is the sum of the subarray starting at 0
   * and ending at i, and count is the number of times we have seen that sum. As explained above, for each sum, we
   * check if sum - k exists in the map, and if it does, increment the result by that value.
   *
   * Time complexity: O(n), since the sum can be calculated in O(1) time, and we calculate the sum once for each element
   * of the array.
   *
   */
  def subarraySum(xs: Seq[Int], k: Int): Int = {
    // (0, 1) is sum of empty array; there's exactly one count of it
    xs.foldLeft((0, 0, collection.mutable.Map[Int, Int]((0, 1)))) { case ((sum, count, sums), i) =>
      val x = sum + i
      val y = count + sums.getOrElse(x - k, 0)
      sums(x) = sums.getOrElse(x, 0) + 1
      (x, y, sums)
    }
      ._2
  }
}

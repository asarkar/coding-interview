package org.asarkar.codinginterview

import scala.annotation.tailrec

package object sorting {
  /*
   * 2-sum
   *
   * ANSWER: We make the following assumptions:
   * 1. Input array contains only distinct elements
   * 2. Order doesn't matter, i.e. (1, 3) = (3, 1)
   * 3. An element can't be paired with itself, i.e. (1, 1) is not a valid answer for k = 2.
   *
   * Time complexity: O(n log(n)), space complexity: O(n) (could be O(1) if we sort in-place)
   */
  def twoSum(xs: IndexedSeq[Int], k: Int): Seq[(Int, Int)] = {
    twoSum(xs.sorted, k, 0, xs.size - 1)
  }

  def twoSum(xs: IndexedSeq[Int], k: Int, start: Int, end: Int): Seq[(Int, Int)] = {
    Iterator.iterate((start, end, collection.mutable.Set.empty[(Int, Int)])) { case (i, j, acc) =>
      if (i < j && xs(i) + xs(j) == k) {
        val x = if (xs(i) < xs(j)) (xs(i), xs(j)) else (xs(j), xs(i))
        if (!acc.contains(x)) acc += x

        (i + 1, j - 1, acc)
      } else if (xs(i) + xs(j) < k) (i + 1, j, acc)
      else (i, j - 1, acc)
    }
      .dropWhile(x => x._1 < x._2)
      .take(1)
      .map(_._3.toSeq)
      .next()
  }

  /*
   * 3-sum
   *
   * ANSWER: Using 2-sum as a subroutine, for each element x, we check for pairs that sum to k - x.
   *
   * Time complexity: O(n²), space complexity: O(n)
   */
  def threeSum(xs: IndexedSeq[Int], k: Int): Seq[(Int, Int, Int)] = {
    threeSum(xs.sorted, k, 0, xs.size - 1)
  }

  def threeSum(xs: IndexedSeq[Int], k: Int, start: Int, end: Int): Seq[(Int, Int, Int)] = {
    (start to end)
      .flatMap(i => twoSum(xs, k - xs(i), i + 1, xs.size - 1)
        .map(x => Seq(xs(i), x._1, x._2).distinct.sorted)
        .filter(_.size == 3)
        .map(zs => (zs.head, zs.tail.head, zs.last))
      )
  }

  /*
   * 4-sum
   *
   * ANSWER: Using 3-sum as a subroutine, for each element x, we check for triplets that sum to k - x.
   *
   * Time complexity: O(n³), space complexity: O(n)
   */
  def fourSum(xs: IndexedSeq[Int], k: Int): Seq[(Int, Int, Int, Int)] = {
    val ys = xs.sorted

    ys.indices
      .flatMap(i => threeSum(ys, k - ys(i), i + 1, ys.size - 1)
        .map(x => Seq(ys(i), x._1, x._2, x._3).distinct.sorted)
        .filter(_.size == 4)
        .map(zs => (zs.head, zs.tail.head, zs.tail.tail.head, zs.last))
      )
  }

  /*
   * Given an array of time intervals (start, end) for classroom lectures (possibly overlapping), find the minimum
   * number of rooms required.
   *
   * For example, given [(30, 75), (0, 50), (60, 150)], you should return 2.
   *
   * ANSWER: We need to find the maximum number of overlapping intervals. To do that, we sort the start and the end
   * times, and run two pointers over them. Whenever a start time is smaller than an end time, a new meeting is
   * starting, and a room is needed. Otherwise, a meeting is ended, and a room freed up. The maximum number of rooms
   * in use at any time is the minimum number of rooms required for all the lectures.
   *
   * Time complexity: O(n), space complexity: O(1)
   */
  def minRooms(lectures: Seq[(Int, Int)]): Int = {
    val starts = lectures
      .map(_._1)
      .sorted
    val ends = lectures
      .map(_._2)
      .sorted

    @tailrec
    def loop(i: Int, j: Int, count: Int, rooms: Int): Int = {
      if (starts.isDefinedAt(i) && ends.isDefinedAt(j)) {
        if (starts(i) < ends(j)) loop(i + 1, j, count + 1, math.max(rooms, count + 1))
        else loop(i, j + 1, count - 1, rooms)
      } else rooms
    }

    loop(0, 0, 0, 0)
  }

  /*
   * Given an array of strictly the characters 'R', 'G' and 'B', segregate the values of the array so that all the Rs
   * come first, the Gs come second, and the Bs come last. You can only swap elements of the array.
   * Do this in linear time and in-place.
   *
   * For example, given the array ['G', 'B', 'R', 'R', 'B', 'R', 'G'], it should become
   * ['R', 'R', 'R', 'G', 'G', 'B', 'B'].
   *
   * ANSWER: We use a modified 3-way partitioning algorithm for the partitioning step.
   */
  def partitionRGB(colors: Array[Char]): Unit = {
    def swap(i: Int, j: Int): Unit = {
      if (i != j) {
        val tmp = colors(i)
        colors(i) = colors(j)
        colors(j) = tmp
      }
    }

    var hi = 0
    var mid = 0
    var lo = colors.length - 1
    val pivot = 'G'

    while (mid <= lo) {
      if (colors(mid) > pivot) {
        swap(mid, hi)
        mid += 1
        hi += 1
      } else if (colors(mid) < pivot) {
        swap(mid, lo)
        // don't increment mid yet since we don't know anything about the element that ended up there
        lo -= 1
      } else mid += 1
    }

    assert(!(0 until lo).exists(colors(_) < pivot),
      s"All elements on the left of index: $lo must be >= $pivot, ${colors.deep}"
    )
    assert(!(hi + 1 until colors.length).exists(colors(_) > pivot),
      s"All elements on the right of index: $hi must be <= $pivot, ${colors.deep}"
    )
  }
}

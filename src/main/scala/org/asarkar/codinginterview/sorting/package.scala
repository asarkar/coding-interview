package org.asarkar.codinginterview

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

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

  /*
   * We can determine how "out of order" an array A is by counting the number of inversions it has. Two elements
   * A[i] and A[j] form an inversion if A[i] > A[j] but i < j. That is, a smaller element appears after a larger
   * element.
   *
   * Given an array, count the number of inversions it has. Do this faster than O(N^2) time.
   * You may assume each element in the array is distinct.
   *
   * For example, a sorted list has zero inversions. The array [2, 4, 1, 3, 5] has three inversions: (2, 1), (4, 1),
   * and (4, 3). The array [5, 4, 3, 2, 1] has ten inversions: every distinct pair forms an inversion.
   *
   * ANSWER: We can solve this problem by modifying the merge step of merge sort to count the number of inversions in
   * addition to merging two sorted arrays. Observe that if arrays A and B are sorted, A[i] > B[j] is an inversion.
   * Not only that, since A is sorted, the elements A[i+1] till the end are also greater than B[j], and count as
   * inversions.
   * Time Complexity: O(n log(n))
   */
  def numInversions(xs: IndexedSeq[Int]): Int = {
    def mergeAndCount(a: IndexedSeq[Int], b: IndexedSeq[Int], i: Int = 0, j: Int = 0): (IndexedSeq[Int], Int) = {
      if (a.isDefinedAt(i) && b.isDefinedAt(j)) {
        if (a(i) > b(j)) {
          val (merged, count) = mergeAndCount(a, b, i, j + 1)
          (b(j) +: merged, count + (a.size - i))
        } else {
          val (merged, count) = mergeAndCount(a, b, i + 1, j)
          (a(i) +: merged, count)
        }
      } else {
        (a.takeRight(a.size - i) ++ b.takeRight(b.size - j), 0)
      }
    }

    def mergeSort(a: IndexedSeq[Int]): (IndexedSeq[Int], Int) = {
      if (a.size <= 1) (a, 0)
      else {
        val mid = a.size / 2
        val (left, right) = a.splitAt(mid)
        val (sorted1, count1) = mergeSort(left)
        val (sorted2, count2) = mergeSort(right)

        val (sorted3, count3) = mergeAndCount(sorted1, sorted2)
        (sorted3, count1 + count2 + count3)
      }
    }

    mergeSort(xs)._2
  }

  /*
   * Given an array A, we can perform a pancake flip: We choose some positive integer k <= A.length, then reverse the
   * order of the first k elements of A.  We want to perform zero or more pancake flips (doing them one after another
   * in succession) to sort the array A.
   * This is also known as pancake sort.
   *
   * ANSWER: The idea is to progressively move the largest element in a range to the end. At each iteration, we find
   * the index of the largest element. We then flip the elements up to and including the largest element, thus putting
   * it in the front. We then flip the whole range, putting the largest element at the end. We decrement the size of
   * range, and repeat, until there is just one element left in the range.
   * Time complexity: In the worst case, the array is reverse sorted, and the largest element is always at the front of
   * the range. The number of reversals performed each time is the size of the range.
   * Time complexity: n + (n - 1) + (n - 2) + ... + 1
   * = n(n + 1)/2
   * = O(n^2)
   */
  def pancakeSort(a: Array[Int]): Unit = {
    @tailrec
    def sort(size: Int): Unit = {
      if (size > 0) {
        val maxIdx = (0 to size)
          .maxBy(a)
        if (maxIdx > 0) reverse(maxIdx)
        reverse(size)
        sort(size - 1)
      }
    }

    def reverse(hi: Int): Unit = {
      (0 to hi / 2)
        .foreach { i =>
          val temp = a(i)
          a(i) = a(hi - i)
          a(hi - i) = temp
        }
    }

    sort(a.length - 1)
  }

  /*
   * Given a set of closed intervals, find the smallest set of numbers that covers all the intervals. If there are
   * multiple smallest sets, return any of them.
   * For example, given the intervals [0, 3], [2, 6], [3, 4], [6, 9], one set of numbers that covers all these
   * intervals is {3, 6}.
   *
   * ANSWER: See smallest-cover.png for a visual description of the problem.
   *
   * We sort the intervals by the end numbers. For any interval, if its start is not greater than the previous end
   * (end is not smaller than the previous end since the intervals have been sorted), then we have an overlap at the
   * previous end, and skip this interval. If the start of the current interval is greater than the previous end,
   * we have no overlap, and add the current end to the result set. We also update previous to the current end.
   *
   * Time complexity: n log(n), where n is the number of intervals.
   */
  def smallestCover(xs: Seq[(Int, Int)]): Seq[Int] = {
    xs
      .sortBy(_._2)
      .foldLeft(ListBuffer.empty[Int]) { case (buffer, (start, end)) =>
        val prev = buffer.lastOption.getOrElse(Int.MinValue)
        if (start <= prev) buffer
        else buffer += end
      }
  }
}

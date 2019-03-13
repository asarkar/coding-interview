package org.asarkar.codinginterview

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

package object arrays {
  /*
   * Count negative integers in row/column-wise sorted matrix
   *
   * ANSWER: If the matrix has m rows and n columns, a brute force solution is to look at all elements of the matrix,
   * time complexity O(mn).
   * A better solution that uses the fact that each row is sorted, is to do a binary search on each row looking for
   * the first negative number. Time complexity is O(m log(n)).
   * However, if we use the fact that not only each row is sorted, but so is each column, we realize that once we find
   * the last negative number in a row, there cannot be any more negative numbers to its right, or below its right.
   * Thus, we start looking from the last element in the first row, and look at each element moving left. Once we find
   * a negative number, we move down by a row, continuing the search until we fall of the ends of the matrix. Time
   * complexity: O(m + n). It's not immediately obvious, but we only have to look at as many elements as the bigger
   * of m and n.
   */
  def countNegative(xs: IndexedSeq[IndexedSeq[Int]]): Int = {
    Iterator.iterate((0, xs.head.size - 1, 0)) { case (row, col, count) =>
      if (!xs.isDefinedAt(row) || !xs(row).isDefinedAt(col)) (-1, -1, count)
      else if (xs(row)(col) < 0) (row + 1, col, count + col + 1)
      else (row, col - 1, count)
    }
      .dropWhile(_._1 >= 0)
      .take(1)
      .next()
      ._3
  }

  /*
   * Randomly reorder array in O(n)
   *
   * ANSWER: We use Fisher-Yates shuffle
   * https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
   */
  def shuffle[T](xs: Array[T]): Unit = {
    for (i <- xs.length - 1 to 1 by -1) {
      val j = Random.nextInt(i + 1)
      if (i != j) {
        val tmp = xs(j)
        xs(j) = xs(i)
        xs(i) = tmp
      }
    }
  }

  /*
   * Tower hopper problem: Given an array of integers A, such that you can advance to (i + j) for 0 <= i < A.length
   * and 1 <= j <= A[i], determine if there exists a series of moves that takes you outside the array.
   * For example, given [4, 2, 0, 0, 2, 0], the moves {0, 4, 2} end with the last position 6 outside the array.
   * However, given [1, 0], no moves exist that end with the last position outside the array.
   *
   * ANSWER: This problem can be solved in a variety of ways:
   *
   * 1) Connectivity in a graph: We represent each index i as a vertex, with edges connecting i and j
   * for 0 <= i < A.length and 1 <= j <= A[i]. We also add a new vertex A.length, with edges connecting it to a vertex i
   * if for some j, 1 <= j <= A[i], i + j >= A.length. Then the problem reduces to finding a path between the vertices
   * 0 and A.length, which can be determined by running BFS from 0.
   *
   * 2) Dynamic programming: Observe that if it's possible to solve the problem from index j, then it is also possible
   * to solve the problem from all i from where we can reach j. Thus, we create a boolean array dp[A.length - 1],
   * where dp[i] = true means it's possible to solve the problem from index i.
   * dp[i] = true if for some j, 1 <= j <= A[i], i + j >= A.length or dp[i + j] = true
   *       = false otherwise   *
   * We start populating dp from A.length - 1, and in the end, simply return the value of dp[0].
   *
   * 3) Greedy approach: For 0 <= i < A.length and 1 <= j <= A[i], choose j such that i + j + A[i + j] is maximized.
   * The idea is to choose the option that gets us the farthest. This is the approach implemented below.
   */
  def isHoppable(towers: IndexedSeq[Int]): Boolean = {
    Iterator.iterate(0) { current =>
      (1 to towers(current))
        .map(_ + current)
        .foldLeft(-1)((max, next) => math.max(max, next + towers.lift(next).getOrElse(0)))
    }
      .dropWhile(towers.isDefinedAt)
      .take(1)
      .next() >= towers.length
  }

  /*
   * Most frequently occurring item in an array
   *
   * ANSWER: Use hashing.
   */

  /*
   * Find common elements in two sorted arrays
   *
   * ANSWER: Two pointers, advance the one that points to the smaller element. If equal, advance both.
   */
  def intersection(xs: IndexedSeq[Int], ys: IndexedSeq[Int]): Seq[Int] = {
    Iterator.iterate((0, 0, collection.mutable.Seq.empty[Int])) { case (i, j, zs) =>
      if (!xs.isDefinedAt(i) || !ys.isDefinedAt(j)) (-1, -1, zs)
      else if (xs(i) == ys(j)) (i + 1, j + 1, zs :+ xs(i))
      else if (xs(i) < ys(j)) (i + 1, j, zs)
      else (i, j + 1, zs)
    }
      .dropWhile(_._1 >= 0)
      .take(1)
      .map(_._3)
      .next()
  }

  /*
   * Rotate an array to the left
   */
  def rotateLeft(xs: IndexedSeq[Int], k: Int): IndexedSeq[Int] = {
    xs.indices
      .map(i => (if (i - k >= 0) i else xs.size) - k)
      .map(xs)
  }

  /*
   * Is one array a rotation of another?
   *
   * ANSWER: Note that B is a rotation of A if B is a subarray of A concatenated with A.
   * Two pointers, i and j:
   * 1) If A[i] == B[j], increment both i and j
   * 2) Increment i, reset j to zero
   *
   * Instead of actually concatenating A with A, if we have found a match, but A ran out, restart from its beginning
   * until one of the following becomes true:
   * 1) i >= 2(A.length)
   * 2) j >= B.length
   */
  def isRotation(xs: IndexedSeq[Int], ys: IndexedSeq[Int]): Boolean = {
    val n = xs.size
    val m = ys.size

    if (n != m) false
    else {
      Iterator.iterate((0, 0)) { case (i, j) =>
        // match
        if (j >= m) (Int.MaxValue, j)
        // rollover
        else if (i >= n) {
          // match
          if (j > 0 && xs(i % n) == ys(j)) (i + 1, j + 1)
          // no match
          else (Int.MaxValue, -1)
        }
        // match
        else if (xs(i) == ys(j)) (i + 1, j + 1)
        // no match
        else (i + 1, 0)
      }
        .dropWhile(x => x._1 < 2 * n || ys.isDefinedAt(x._2))
        .take(1)
        .map(_._2 >= m)
        .next()
    }
  }

  /*
   * Rotate a 2D array by 90 degrees
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/arrays/package.scala
   */

  /*
   * Best Time to Buy and Sell Stock, if you're allowed one transaction. You may not engage in multiple
   * transactions at the same time (ie, you must sell the stock before you buy again).
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/arrays/package.scala
   */

  /*
   * Best Time to Buy and Sell Stock, if you're allowed unlimited transactions. You may not engage in multiple
   * transactions at the same time (ie, you must sell the stock before you buy again).
   *
   * ANSWER: Sum all the positive differences. In other words, sell whenever the price is greater than the buying price.
   */
  def stocks2(prices: IndexedSeq[Int]): Int = {
    (1 until prices.size)
      .filter(i => prices(i) > prices(i - 1))
      .map(i => prices(i) - prices(i - 1))
      .sum
  }

  /*
   * Rotate an array k positions to the right
   *
   * ANSWER: We rotate the array in place. Observe that the target position of every element is given by
   * (index + k) modulo size.
   * For range 0 to k - 1, we recursively swap each element with the one in its target position as long as the target
   * position is greater than the current position. This is because since we are incrementally progressing from lower
   * to higher indices, a smaller target index indicates that the corresponding element had already been swapped.
   *
   * Example:
   * Rotate [1, 2, 3, 4, 5, 6] by 3
   *
   * Index to target index:
   * 0 to 3
   * 1 to 4
   * 2 to 5
   * 3 to 0
   * 4 to 1
   * 5 to 2
   *
   * swap(0, 3) => [4, 2, 3, 1, 5, 6]
   * swap(0, 0) => return
   * swap(1, 4) => [4, 5, 3, 1, 2, 6]
   * swap(1, 1) => return
   * swap(2, 5) => [4, 2, 6, 1, 2, 3]
   * swap(2, 2) => return
   *
   * Done!
   *
   * Another example:
   * Rotate [2, 3, 4, 1] by 1
   *
   * Index to target index:
   * 0 to 1
   * 1 to 2
   * 2 to 3
   * 3 to 0
   *
   * swap(0, 1) => [3, 2, 4, 1]
   * swap(0, 2) => [4, 2, 3, 1]
   * swap(0, 3) => [1, 2, 3, 4]
   * swap(3, 0) => return
   *
   * Done!
   */
  def rotateRight(xs: Array[Int], k: Int): Unit = {
    @tailrec
    def swap(original: Int, current: Int): Unit = {
      val target = (original + k) % xs.length

      if (target > current) {
        val tmp = xs(current)
        xs(current) = xs(target)
        xs(target) = tmp

        swap(target, current)
      }
    }

    xs.indices
      .take(k)
      .foreach(i => swap(i, i))
  }

  object Move extends Enumeration {
    type Move = Value
    val X = Value(1)
    val O = Value(-1)
    val Empty = Value(0)
  }

  import Move._

  /*
   * Given a 3x3 Tic-Tac-Toe board, decide if there's a winner
   *
   * ANSWER: Check every row, column and diagonals.
   */
  def ticTacToeWinner(board: IndexedSeq[IndexedSeq[Move]]): Option[Move] = {
    // since player X makes the first move, num X must be equal to num O or at most greater by one
    val a = board.indices
      .flatMap(row => board.indices.map(col => board(row)(col).id))
      .sum
    require(a == 0 || a == 1, "Invalid board")

    // check rows
    var winner = board.indices
      .find(row => math.abs(board(row).map(_.id).sum) == board.size) match {
      case Some(row) =>
        val sum = board(row).map(_.id).sum
        if (sum > 0) Some(X) else Some(O)
      case _ => None
    }

    // check columns
    winner = winner match {
      case None => board.indices
        .find(col => math.abs(board.indices.foldLeft(0)((sum, row) => sum + board(row)(col).id)) == board.size) match {
        case Some(col) =>
          val sum = board.indices.foldLeft(0)((sum, row) => sum + board(row)(col).id)
          if (sum > 0) Some(X) else Some(O)
        case _ => None
      }
      case x => x
    }

    // check downward diagonal
    winner = winner match {
      case None =>
        val sum = board.indices.zip(board.indices)
          .foldLeft(0) { case (total, (row, col)) => total + board(row)(col).id }
        if (math.abs(sum) == board.size) if (sum > 0) Some(X) else Some(O)
        else None
      case x => x
    }

    // check upward diagonal
    winner match {
      case None =>
        val sum = board.indices.zip(board.indices.reverse)
          .foldLeft(0) { case (total, (row, col)) => total + board(row)(col).id }
        if (math.abs(sum) == board.size) if (sum > 0) Some(X) else Some(O)
        else None
      case x => x
    }
  }

  /*
   * Matrix spiral
   *
   * ANSWER: The idea is to print the peripheral elements, and move in from each side.
   */
  def matrixSpiral(xs: IndexedSeq[IndexedSeq[Int]]): Seq[Int] = {
    val rows = xs.size
    val cols = xs.headOption.map(_.size).getOrElse(0)

    def loop(row: Int, col: Int): Seq[Int] = {
      val lastRow = rows - row - 1
      val lastCol = cols - col - 1

      if (row > lastRow || col > lastCol) Seq.empty[Int]
      else {
        val ys =
        // top
          (col to lastCol)
            .map(c => xs(row)(c)) ++
            // right; since top loop already included the top right element, exclude it; also exclude bottom right
            // element because it's included in the bottom loop
            (row + 1 until lastRow)
              .map(r => xs(r)(lastCol)) ++
            // bottom
            (
              if (row < lastRow) (lastCol to col by -1).map(c => xs(lastRow)(c))
              else Seq.empty[Int]
              ) ++
            // left; exclude bottom left and top left elements
            (
              if (col < lastCol) (lastRow - 1 until row by -1).map(r => xs(r)(col))
              else Seq.empty[Int]
              )

        ys ++ loop(row + 1, col + 1)
      }
    }

    loop(0, 0)
  }

  /*
   * Given an array of integers, return a new array such that each element at index i of the new array is the product
   * of all the numbers in the original array except the one at i. For example, if our input was [1, 2, 3, 4, 5], the
   * expected output would be [120, 60, 40, 30, 24]. If our input was [3, 2, 1], the expected output would be [2, 3, 6].
   *
   * Follow-up: what if you can't use division?
   *
   * ANSWER: See https://github.com/asarkar/adm/blob/master/src/main/scala/org/asarkar/adm/data/package.scala
   */

  /* Given an array of integers, find the first missing positive integer in linear time and constant space.
   * In other words, find the lowest positive integer that does not exist in the array.
   * The array can contain duplicates and negative numbers as well.
   * For example, the input [3, 4, -1, 1] should give 2. The input [1, 2, 0] should give 3.
   * You can modify the input array in-place.
   *
   * ANSWER: We make a crucial observation: if the input array is partitioned such that positive numbers precede
   * the negative numbers, the missing positive number is an integer between 1 and n + 1, where n is the index
   * of the last positive number. This is because if the array had all integers from 1 to n, then the missing integer
   * would be n + 1. We use a modified 3-way partitioning algorithm for the partitioning step.
   *
   * We need some way to mark the integers that are present. If we were allowed to used O(n) space, we could have a
   * boolean array of length n + 1, and set the indices corresponding to the values found in the input array to true.
   * Since we are not allowed to use additional space, we will modify the input array to indicate which integers are
   * present. We do so by walking the array, and for 0 < i <= n, we set A[i - 1] = -A[i - 1]. We then walk the array,
   * again and when we find a positive value, return its index + 1 as the answer.
   */
  def firstMissingPositiveNumber(xs: Array[Int]): Int = {
    def swap(i: Int, j: Int): Unit = {
      if (i != j) {
        val tmp = xs(i)
        xs(i) = xs(j)
        xs(j) = tmp
      }
    }

    var hi = 0
    var mid = 0
    var lo = xs.length - 1
    val pivot = 0

    while (mid < lo) {
      if (xs(mid) > pivot) {
        swap(mid, hi)
        mid += 1
        hi += 1
      } else if (xs(mid) < pivot) {
        swap(mid, lo)
        lo -= 1
      } else mid += 1
    }

    // if xs(mid) < 0, there are no positive numbers in the array, otherwise mid points to the last non-negative number
    (0 to mid)
      .filter(i => xs(i) > 0 && xs(i) <= (mid + 1))
      .foreach { i =>
        val x = math.abs(xs(i))
        // may already be negative if there're duplicates in the array
        if (xs(x - 1) > 0) xs(x - 1) *= -1
      }

    (0 to mid)
      .find(i => xs(i) > 0)
      .getOrElse(if (xs(mid) < 0) 0 else mid) + 1
  }

  /*
   * You are given frames from video segments. Each frame has a unique id. Your task is to find the length of each
   * video segment.
   *
   * Examples:
   * Given the frames [a, b, c, d], there are four segments of unit length.
   * Given the frames [a, b, c, a], there is one segment of length 4.
   * Given the frames [a, b, c, a, b, d, e], there are three segments of lengths 5, 1 and 1, respectively.
   */
  def segmentLengths(xs: Seq[Char]): Seq[Int] = {
    val indexMap = xs
      .zipWithIndex
      .toMap // Duplicate keys will be overwritten by later keys

    def findEnd(start: Int, end: Int, idx: Int): Seq[Int] = {
      if (xs.isDefinedAt(start))
        if (idx <= end) findEnd(start, math.max(end, indexMap(xs(idx))), idx + 1)
        else Seq(end - start + 1) ++ findEnd(end + 1, end + 1, end + 1)
      else Seq.empty
    }

    findEnd(0, 0, 0)
  }

  /*
   * Given an array of integers and a number k, where 1 <= k <= length of the array, compute the maximum values of each
   * subarray of length k.
   *
   * For example, given array = [10, 5, 2, 7, 8, 7] and k = 3, we should get: [10, 7, 8, 8], since:
   * 10 = max(10, 5, 2)
   * 7 = max(5, 2, 7)
   * 8 = max(2, 7, 8)
   * 8 = max(7, 8, 7)
   *
   * Do this in O(n) time and O(k) space. You can modify the input array in-place and you do not need to store the
   * results. You can simply print them out as you compute them.
   *
   * ANSWER: We want the maximum in a sliding window of size k that moves by a unit each time.
   * We use a queue with two invariants:
   * 1) The queue only stores indices in the current window.
   * 2) Indices in the queue are sorted in decreasing order with respect to their corresponding elements.
   *
   * At each iteration, the maximum is the element at the top of the queue. To maintain invariant #1, we remove
   * all indices from the front of queue that are outside the current window. To maintain invariant #2, we remove
   * all indices from the end of the queue that correspond to elements smaller than the current element.
   *
   * Note that although each element may be compared multiple times, it is enqueued and dequeued at most once.
   * Thus, overall time complexity is O(n), and space complexity is O(k).
   */
  def maxValuesOfSubarrays(xs: IndexedSeq[Int], k: Int): Seq[Int] = {
    if (k <= 0 || k > xs.size) Seq.empty[Int]
    else {
      println(s"Given sequence: $xs")
      val queue = ListBuffer.empty[Int]
      val numWindows = xs.size - k + 1
      val maximums = Array.ofDim[Int](numWindows)

      for (i <- xs.indices) {
        println(s"Queue: $queue")
        if (i >= k && queue.nonEmpty) {
          println(s"The maximum element in the window: [${i - k}, $i) is at index: ${queue.head}")
          maximums(i - k) = xs(queue.head)
        }
        while (queue.lastOption.exists(j => xs(j) < xs(i))) {
          println(s"Removing index: ${queue.last} as the corresponding element is smaller than the incoming element " +
            s"at index: $i")
          queue.remove(queue.size - 1)
        }
        while (queue.headOption.exists(_ <= (i - k))) {
          println(s"Removing index: ${queue.head} as it is outside the current window: [$i, ${i + k})")
          queue.remove(0)
        }

        println(s"Adding index: $i")
        queue.append(i)
      }

      maximums(numWindows - 1) = xs(queue.head)
      maximums
    }
  }
}

package org.asarkar.codinginterview

import scala.annotation.tailrec
import scala.collection.Searching._
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

  /* Given an array of integers, find the first missing positive integer in linear time and constant space.
   * In other words, find the lowest positive integer that does not exist in the array.
   * The array can contain duplicates and negative numbers as well.
   * For example, the input [3, 4, -1, 1] should give 2. The input [1, 2, 0] should give 3.
   * You can modify the input array in-place.
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/honors/package.scala
   */

  /*
   * Given an array of integers, return a new array such that each element at index i of the new array is the product
   * of all the numbers in the original array except the one at i. For example, if our input was [1, 2, 3, 4, 5], the
   * expected output would be [120, 60, 40, 30, 24]. If our input was [3, 2, 1], the expected output would be [2, 3, 6].
   *
   * Follow-up: what if you can't use division?
   *
   * ANSWER: See https://github.com/asarkar/adm/blob/master/src/main/scala/org/asarkar/adm/data/package.scala
   */

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

  /*
   * You are given an array of non-negative integers that represents a two-dimensional elevation map where each element
   * is unit-width wall and the integer is the height. Suppose it will rain and all spots between two walls get filled
   * up.
   * Compute how many units of water remain trapped on the map in O(N) time and O(1) space.
   *
   * For example, given the input [2, 1, 2], we can hold 1 unit of water in the middle.
   * Given the input [3, 0, 1, 3, 0, 5], we can hold 3 units in the first index, 2 in the second, and 3 in the fourth
   * index (we cannot hold 5 since it would run off to the left), so we can trap 8 units of water.
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/honors/package.scala
   */

  /*
   * Transpose a matrix.
   *
   * ANSWER: Row becomes column, or column becomes row
   */
  def transposeMatrix(xs: Array[Array[Int]]): Unit = {
    val n = xs.length
    for (layer <- 0 until n; i <- layer + 1 until n) {
      val tmp = xs(layer)(i)
      xs(layer)(i) = xs(i)(layer)
      xs(i)(layer) = tmp
    }
  }

  /*
   * Get product of all other elements.
   *
   * ANSWER: See https://github.com/asarkar/adm/tree/master/src/main/scala/org/asarkar/adm/data/package.scala
   */

  /*
   * Given an integer array, you need to find one continuous subarray that if you only sort this subarray in ascending
   * order, then the whole array will be sorted in ascending order, too.
   * You need to find the shortest such subarray and output its length.
   *
   * ANSWER: Observe that any element that is to the right of and smaller than the maximum element seen so far is in
   * the wrong place. Thus, if we keep track of the running max, and the index of the last element that's smaller than
   * it, we will have the right boundary of the subarray that needs to be sorted. Lets name it right.
   * Similarly, we can find the left boundary by keeping track of the index of the last element that's to the left of
   * and greater than the running min. We name it left.
   *
   * The answer is simply the size of the subarray between left and right, both inclusive. If the array is already
   * sorted, then left is not less than right, and we return zero.
   */
  def unsortedSubarrayLen(xs: IndexedSeq[Int]): Int = {
    if (xs.isEmpty) 0
    else {
      val r = (1 until xs.size)
        .foldLeft((xs.head, -1)) { case ((max, right), i) =>
          if (xs(i) < max) (max, i)
          else if (xs(i) > max) (xs(i), right)
          else (max, right)
        }
        ._2

      val l = (xs.size - 2 to 0 by -1)
        .foldLeft((xs.last, -1)) { case ((min, left), i) =>
          if (xs(i) > min) (min, i)
          else if (xs(i) < min) (xs(i), left)
          else (min, left)
        }
        ._2

      if (r > l) r - l + 1 else 0
    }
  }

  /*
   * You are given an integer array nums and you have to return a new counts array. The counts array has the property
   * where counts[i] is the number of smaller elements to the right of nums[i].
   *
   * For example:
   * nums: [5,2,6,1]
   * count: [2,1,1,0]
   *
   * Explanation:
   * To the right of 5 there are 2 smaller elements (2 and 1).
   * To the right of 2 there is only 1 smaller element (1).
   * To the right of 6 there is 1 smaller element (1).
   * To the right of 1 there is 0 smaller element.
   *
   * ANSWER: Note that in a list sorted in the descending order, the number of elements smaller than any element are
   * the ones on its right. Thus, if we build this list starting from the last element of nums, at each iteration,
   * we can find the index where the current element is supposed to be inserted to preserve the sorting. We can find
   * this insertion index using a modified binary search. If there are equal elements, we insert at the end, consistent
   * with the idea that all elements to the right of the newly inserted element must be smaller than itself.
   * Python happens to have a library for this called bisect (https://docs.python.org/2/library/bisect.html).
   *
   * We use a ListBuffer for the sorted list which internally uses a LinkedList. Insertion in the worst case takes O(n)
   * time. Binary search takes O(log n) time. Thus, the overall time complexity is O(n^2).
   *
   * A better solution would be to use a BST instead of a LinkedList where the left subtree of the BST consists of
   * strictly smaller elements. If we keep track of the size of the left subtree at each node, it is precisely the
   * number of elements smaller than the current element. Insertion in BST takes O(log n) time, and so does finding a
   * node. Thus, overall time complexity would be O(n log n). Of course, if the input array is sorted, BST would
   * degenerate in a linked list. An even better solution would be to use a Red-black tree.
   */
  def countSmaller(nums: IndexedSeq[Int]): IndexedSeq[Int] = {
    @tailrec
    def lastIndex(i: Int, lo: Int, hi: Int, sorted: ListBuffer[Int]): Int = {
      if (lo > hi) lo
      else {
        val mid = lo + (hi - lo) / 2
        if (sorted(mid) == i) {
          if (mid + 1 <= hi && (sorted(mid + 1) == i)) lastIndex(i, mid + 1, hi, sorted)
          else mid + 1
        } else if (sorted(mid) < i) lastIndex(i, lo, mid - 1, sorted)
        else lastIndex(i, mid + 1, hi, sorted)
      }
    }

    (nums.size - 1 to 0 by -1)
      .foldLeft((Array.ofDim[Int](nums.size), ListBuffer.empty[Int])) { case ((count, sorted), i) =>
        val idx = lastIndex(nums(i), 0, sorted.size - 1, sorted)
        sorted.insert(idx, nums(i))
        count(i) = math.max(0, sorted.size - 1 - idx)
        (count, sorted)
      }
      ._1
  }

  /*
   * On our special chessboard, two bishops attack each other if they share the same diagonal. This includes bishops
   * that have another bishop located between them, i.e. bishops can attack through pieces.
   * You are given N bishops, represented as (row, column) tuples on a M by M chessboard. Write a function to count
   * the number of pairs of bishops that attack each other. The ordering of the pair doesn't matter: (1, 2) is
   * considered the same as (2, 1).
   *
   * For example, given M = 5 and the list of bishops:
   * (0, 0), (1, 2), (2, 2), (4, 0), (4, 4)
   * The board would look like this:
   * [b 0 0 0 0]
   * [0 0 b 0 0]
   * [0 0 b 0 0]
   * [0 0 0 0 0]
   * [b 0 0 0 b]
   *
   * You should return 2, since bishops 1 and 3 attack each other, as well as bishops 3 and 4.
   *
   * ANSWER: We simply launch a search from each cell that contains a bishop. The efficiency comes from not searching
   * along a diagonal twice. In the example above, we would visit cells (1, 1), (2, 2), (3, 3) and (4, 4) while looking
   * for another bishop under attack from the one at (0, 0). Later, when searching for bishops under attack from (2, 2),
   * there's no need to search this diagonal again. Same goes for (4, 4). The number of attacking pairs along a
   * diagonal is simply k choose 2, where k is the number of bishops along that diagonal.
   *
   * Time complexity:
   * If there are 2N bishops, placed along the top row and the left column, we will end up visiting all the cells.
   * Thus, worst case time complexity is O(N^2). Space complexity is always O(N^2).

Hence in all cases the number of squares touched is

𝑁+𝑀−(𝑁,𝑀)
   */
  object ContentOfCell extends Enumeration {
    val Bishop, Empty, Seen = Value
  }

  def numAttackingBishops(bishops: Seq[(Int, Int)], dim: Int): Int = {
    val board = Array.fill[ContentOfCell.Value](dim, dim)(ContentOfCell.Empty)
    bishops
      .foreach { case (r, c) => board(r)(c) = ContentOfCell.Bishop }

    def count(row: Int, col: Int, f: Int => (Int, Int)): Int = {
      val cnt = Iterator.range(1, dim)
        .map(f)
        .takeWhile { case (r, c) => board.isDefinedAt(r) && board(r).isDefinedAt(c) && board(r)(c) != ContentOfCell.Seen }
        .foldLeft(0) { case (acc, (r, c)) =>
          val x = if (board(r)(c) == ContentOfCell.Bishop) 1
          else {
            board(r)(c) = ContentOfCell.Seen
            0
          }
          acc + x
        } + 1 // count the bishop at (row, col)
      (cnt * (cnt - 1)) / 2
    }

    bishops
      .map { case (row, col) =>
        count(row, col, j => (row + j, col + j)) +
          count(row, col, j => (row + j, col - j)) +
          count(row, col, j => (row - j, col + j)) +
          count(row, col, j => (row - j, col - j))
      }
      .sum
  }

  /*
   * Given a list of integers, return the largest product that can be made by multiplying any three integers.
   * For example, if the list is [-10, -10, 5, 2], we should return 500, since that's -10 * -10 * 5.
   * You can assume the list has at least three integers.
   *
   * ANSWER: We scan the array and keep track of the max 3 elements, as well as the minimum 2 elements. This is because
   * in a sorted array, the largest product may come from the max 3 elements, but also from the max element and the
   * min 2 elements (rotate around).
   *
   * Time complexity: O(n).
   */
  def largestProduct(xs: IndexedSeq[Int]): Int = {
    val x = xs.foldLeft((Int.MinValue, Int.MinValue, Int.MinValue, Int.MaxValue, Int.MaxValue)) {
      case ((max1, max2, max3, min1, min2), i) =>
        if (i > max1 && i < min1) (i, i, i, i, i)
        else if (i > max1) (i, max1, max2, min1, min2)
        else if (i > max2) (max1, i, max2, min1, min2)
        else if (i > max2) (max1, max2, i, min1, min2)
        else if (i < min1) (max1, max2, max3, i, min1)
        else if (i < min2) (max1, max2, max3, min1, i)
        else (max1, max2, max3, min1, min2)
    }

    math.max(x._1 * x._2 * x._3, x._4 * x._5 * x._1)
  }

  /*
   * Given an array of numbers, find the length of the longest increasing subsequence in the array. The subsequence
   * does not necessarily have to be contiguous.
   *
   * For example, given the array [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15], the longest increasing
   * subsequence has length 6: it is 0, 2, 6, 9, 11, 15.
   *
   * ANSWER: One solution that comes to might is keeping an array dp, where dp[i] is the length of the longest
   * increasing subsequence ending at i.
   * dp[i] = max {dp[j]}, 0 <= j < i, nums[j] < nums[i].
   *
   * Its easy to see that this is a O(n^2) algorithm. Can we do better?
   *
   * Yes, we can! What if, for each element, we maintain a sorted list that ends with that element? Then the longest
   * increasing subsequence is simply the longest among all such sequences.
   *
   * For example, let the input array be [3, 1, 5, 2, 6, 4, 9]. S be a list, and max = -1 be the length of the longest
   * increasing subsequence (actually the last index of the longest increasing subsequence, as shown below).
   *
   * 3: append to S. S = [3], max = max{-1, 0} = 0.
   * 1: find the insertion point for 1. S = [1, 3], max = max{0, 0} = 0.
   * 5: append to S. S = [1, 3, 5], max {0, 2} = 2.
   * 2: find the insertion point for 2. S = [1, 2, 3, 5], max = max{2, 1} = 2.
   * ...
   *
   * In the end, we return max + 1.
   *
   * Since S is sorted, we can use binary search to find the insertion points. Overall time complexity: O(n log(n)).
   */
  def longestIncSubseq(xs: IndexedSeq[Int]): Int = {
    if (xs.size <= 1) xs.size
    else {
      val subseq = ListBuffer(xs.head)

      xs.tail
        .foldLeft(-1) { (max, i) =>
          val last = subseq.last

          val insertionIndex = if (i > last) (subseq += i).size - 1
          else {
            val j = subseq.search(i).insertionPoint
            subseq(j) = i
            j
          }

          math.max(max, insertionIndex + 1)
        }
    }
  }

  /*
   * You are given an N by M 2D matrix of lowercase letters. Determine the minimum number of columns that can be removed
   * to ensure that each row is ordered from top to bottom lexicographically. That is, the letter at each column is
   * lexicographically later as you go down each row. It does not matter whether each row itself is ordered
   * lexicographically.
   *
   * For example, given the following table:
   * cba
   * daf
   * ghi
   *
   * This is not ordered because of the a in the center. We can remove the second column to make it ordered:
   * ca
   * df
   * gi
   *
   * So your function should return 1, since we only needed to remove 1 column.
   *
   * As another example, given the following table:
   * abcdef
   *
   * Your function should return 0, since the rows are already ordered (there's only one row).
   *
   * As another example, given the following table:
   * zyx
   * wvu
   * tsr
   *
   * Your function should return 3, since we would need to remove all the columns to order it.
   *
   * ANSWER: Probably the simplest question ever asked by Google :) Check characters in each row that are in the same
   * columns; terminate early if necessary.
   */
  def minColToRemove(xs: IndexedSeq[IndexedSeq[Char]]): Int = {
    xs match {
      case first +: next +: rest =>
        val c = first.zip(next)
          .count(x => x._1 > x._2)
        c + (if (c < first.size) minColToRemove(rest) else 0)
      case _ => 0
    }
  }

  /* Given an array of integers, write a function to determine whether the array could become non-decreasing by
   * modifying at most 1 element.
   *
   * For example, given the array [10, 5, 7], you should return true, since we can modify the 10 into a 1 to make the
   * array non-decreasing.
   *
   * Given the array [10, 5, 1], you should return false, since we can't modify any one element to get a non-decreasing
   * array.
   *
   * ANSWER: Let k be the unique index for which xs(k) > xs(k + 1). If this is not unique or doesn't exist, the answer
   * is false or true respectively. We analyze the following cases:
   *
   * - if k = 0, then we could make the array good by setting xs(p) = xs(p + 1).
   * - if k = xs.size - 2, then we could make the array good by setting xs(k + 1) = xs(k).
   * - otherwise, xs is defined for the range [k - 1, k + 2], and:
   * -- we could change xs(k) to be between xs(k - 1) and xs(k + 1) if possible, or;
   * -- we could change xs(k + 1) to be between xs(k) and xs(k + 2) if possible.
   *
   * Time complexity: O(n).
   */
  def checkPossibility(xs: IndexedSeq[Int]): Boolean = {
    @tailrec
    def loop(k: Int, i: Int): Int = {
      if (i > xs.size - 2) k
      else if (xs(i) > xs(i + 1)) {
        if (k >= 0) xs.size
        else loop(i, i + 1)
      }
      else loop(k, i + 1)
    }

    val k = loop(-1, 0)
    xs.isDefinedAt(k) && (k <= 0 || k == xs.size - 2 || xs(k - 1) <= xs(k + 1) || xs(k) <= xs(k + 2))
  }

  /*
   * Given an integer list where each number represents the number of hops you can make, determine whether you can
   * reach to the last index starting at index 0.
   *
   * For example, [2, 0, 1, 0] returns True while [1, 1, 0, 1] returns False.
   *
   * ANSWER: We adapt a greedy strategy such that we jump to the position that takes us closest to the end. In other
   * words, we jump to the position from where we can jump the farthest.
   * Time complexity: O(n).
   */
  def canJump(xs: Seq[Int]): Boolean = {
    Iterator.iterate((0, false)) { case (curr, stuck) =>
      if (xs(curr) == 0) (curr, true)
      else (curr + (1 to xs(curr))
        .maxBy(hop => curr + hop + xs.lift(curr + hop).getOrElse(xs.size)), stuck)
    }
      .dropWhile(x => x._1 < xs.size - 1 && !x._2)
      .take(1)
      .map(_._1)
      .next() >= xs.size - 1
  }

  /*
   * Given a sorted list of integers, square the elements and give the output in sorted order.
   * For example, given [-9, -2, 0, 2, 3], return [0, 4, 4, 9, 81].
   *
   * ANSWER: We find the index of the first positive number (if any), and then start comparing pairs while moving
   * in opposite directions.
   *
   * Time complexity: O(n). In the worst case, all numbers are negative.
   */
  def sortedSquares(xs: Seq[Int]): Seq[Int] = {
    val i = xs.indices
      .find(xs(_) > 0)
      .getOrElse(xs.size)

    Iterator.iterate(i - 1, i, ListBuffer.empty[Int]) { case (j, k, squares) =>
      if (xs.isDefinedAt(j) && xs.isDefinedAt(k)) {
        if (math.abs(xs(j)) < math.abs(xs(k))) (j - 1, k, squares += (xs(j) * xs(j)))
        else (j, k + 1, squares += (xs(k) * xs(k)))
      } else if (xs.isDefinedAt(j)) (j - 1, k, squares += (xs(j) * xs(j)))
      else if (xs.isDefinedAt(k)) (j, k + 1, squares += (xs(k) * xs(k)))
      else (j, k, squares)
    }
      .dropWhile(_._3.size < xs.size)
      .take(1)
      .map(_._3)
      .next()
  }
}

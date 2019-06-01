package org.asarkar.codinginterview

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

package object combinatorial {
  /*
   * All subsets of a set
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/recursion/package.scala
   */

  /*
   * Power set
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/recursion/package.scala
   */

  /*
   * Given a function that generates perfectly random numbers between 1 and k (inclusive), where k is an input, write
   * a function that shuffles a deck of cards represented as an array using only swaps.
   * It should run in O(N) time.
   * Hint: Make sure each one of the 52! permutations of the deck is equally likely.
   *
   * ANSWER: Use Fisher-Yates shuffle.
   * for i from 0 to n - 2 do
   *   j <- random integer such that i <= j < n
   *   exchange a[i] and a[j]
   */

  /*
   * Given an integer n and a list of integers l, write a function that randomly generates a number from 0 to n-1 that
   * isn't in l (uniform).
   *
   * ANSWER: https://stackoverflow.com/a/6443346/839733
   */
  def randomWithExclusion(n: Int, xs: Seq[Int]): Int = {
    val x = new Random().nextInt(n - xs.size)
    val it = xs.sorted
      .iterator
      .zipWithIndex
      .map { y => (x + y._2, y._1) }
      .dropWhile(y => y._1 >= y._2)
      .take(1)
      .map(_._1)
    if (it.hasNext) it.next() else x + xs.size
  }

  /*
   * Given a number represented by a list of digits, find the next greater permutation of a number, in terms of
   * lexicographic ordering. If there is not greater permutation possible, return the permutation with the lowest
   * ordering.
   *
   * For example, the list [1,2,3] should return [1,3,2]. The list [1,3,2] should return [2,1,3]. The list [3,2,1]
   * should return [1,2,3].
   *
   * Can you perform the operation without allocating extra memory (disregarding the input memory)?
   *
   * ANSWER: Starting from the end, we try to find a pair such that the first is smaller than the second. If one such
   * pair exists, we scan all elements to its right to find the minimum (say at index j) that's greater the first
   * element in the pair (say at index i), and swap them.
   * This works for [1,2,3], but for [1,3,2], this produces the array [2,3,1], which is not the next permutation
   * (next is [2,1,3]). We further observe that all elements to the right of i must already be in descending order,
   * and the swap didn't change that property, so simply reversing them produces the next sequence.
   *
   * If no such pair is found, that means the array is sorted in descending order, so we reverse the whole array.
   *
   * Time complexity: O(n). Each element is compared only once to the one on its right. Finding the minimum among the
   * greaters is a linear scan. The swap loop runs n/2 times in worst case, for an array sorted in descending order.
   *
   * Space complexity: O(1).
   *
   * c.f.: https://leetcode.com/problems/next-permutation/solution/
   */
  def nextPermutation(xs: Array[Int]): Unit = {
    def swap(i: Int, j: Int): Unit = {
      val tmp = xs(i)
      xs(i) = xs(j)
      xs(j) = tmp
    }

    val x = (xs.length - 2 to 0 by -1)
      .find(i => xs(i) < xs(i + 1)) match {
      case Some(j) =>
        val k = (j + 1 until xs.length)
          .filter(xs(_) > xs(j))
          .minBy(xs)
        swap(j, k)
        j + 1
      case _ => 0
    }
    (0 to (xs.length - x) / 2)
      .map(i => (x + i, xs.length - i - 1))
      .takeWhile(i => i._1 < i._2)
      .foreach((swap _).tupled)
  }

  /*
   * Given a number in the form of a list of digits, return all possible permutations.
   *
   * For example, given [1,2,3], return [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]].
   *
   * ANSWER: Each digit can either be at the first position, or not. Thus, we put each of the digits in the first
   * position, and recursively append to it the permutations of the remaining digits.
   *
   * Time complexity: O(n!).
   */
  def allPermutations(xs: Array[Int]): Seq[Seq[Int]] = {
    def permute(all: mutable.Buffer[Seq[Int]], prefix: Seq[Int], nums: Array[Int]): mutable.Buffer[Seq[Int]] = {
      if (nums.isEmpty) all += prefix
      else {
        nums.indices
          .foldLeft(all) { (perm, i) =>
            val copy = Array.ofDim[Int](nums.length)
            Array.copy(nums, 0, copy, 0, nums.length)

            val tmp = copy.head
            copy(0) = copy(i)
            copy(i) = tmp

            val copy2 = Array.ofDim[Int](copy.length - 1)
            Array.copy(copy, 1, copy2, 0, copy2.length)
            permute(perm, prefix :+ copy.head, copy2)
          }
      }
    }

    if (xs.isEmpty) Seq.empty
    else permute(ListBuffer.empty, Seq.empty, xs)
  }
}

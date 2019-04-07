package org.asarkar.codinginterview

import scala.annotation.tailrec

package object searching {
  /*
   * Given a graduated income tax system as follows:
   * Up to 10000 is taxed at 10%
   * 10001 to 50000 is taxed at 20%
   * 50001 and above is taxed at 30%
   *
   * Write a program to calculate the taxes on a given income.
   *
   * Examples:
   * Taxes on 15000 would be (20% of 5000 + 10% of 10000) = 2000
   * Taxes on 60000 would be (30% of 10000 + 20% of 40000 + 10% of 10000) = 12000
   *
   * ANSWER:
   * Let N be the number of brackets.
   *
   * NAIVE APPROACH: Linear search for the appropriate tax bracket, compute tax on the excess income in the bracket,
   * and then recursively compute the taxes on the taxable income below the bracket. For example, income 15000 falls
   * in the bracket that starts at 10001; the taxes for this bracket is (15000 - 10000) * 0.2 = 1000 + taxes on 10000.
   * This works, but linear search may take O(N) in the worst case, and if the initial income falls in the highest
   * bracket, the code would loop N times. We end up with a O(N^2) algorithm.
   *
   * BETTER APPROACH: Binary search for the bracket, and then proceed as in the naive approach. O(N log(N)).
   *
   * DYNAMIC PROGRAMMING APPROACH: This problem exhibits both of the two criteria for applying dynamic programming,
   * optimal substructure, and overlapping subproblems. Why? The total taxes for each bracket is the sum of the taxes
   * for the current bracket, and the taxes for the remaining taxable income. For each bracket, the recursive solution
   * computes the taxes for the lower brackets over and over again.
   *
   * Thus, we precompute and memoize the taxes for the taxable income up to the previous bucket in a bottom-up fashion.
   * This takes O(N) time. Binary search for the bracket takes log(N) time. Computing the taxes now takes O(1) times,
   * giving us a linear time algorithm overall.
   *
   */
  def taxes(income: Int, brackets: IndexedSeq[(Int, Double)]): Double = {
    val dp = brackets
      .zipWithIndex
      .foldLeft((0d, IndexedSeq.empty[(Int, Double, Double)])) { case ((sum, acc), (cur, i)) =>
        val taxesOnPrevBracket = if (i > 0) {
          val prev = brackets(i - 1)
          (cur._1 - prev._1) * prev._2
        } else 0d
        val cumulativeTaxes = sum + taxesOnPrevBracket

        (cumulativeTaxes, acc :+ (cur._1, cur._2, cumulativeTaxes))
      }
      ._2

    @tailrec
    def findBracket(start: Int, end: Int): Int = {
      if (end - start <= 1) start
      else {
        val mid = start + (end - start) / 2
        if (income > brackets(mid)._1) findBracket(mid, end)
        else findBracket(start, mid)
      }
    }

    val br = dp(findBracket(0, brackets.size - 1))
    val inc = income - br._1 + 1
    val tx = inc * br._2 + br._3
    println(s"Taxable income: $income, bracket: $br, taxes: $tx")
    tx
  }

  /*
   * Given a sorted array of integers, and a range [lo, hi], return the number of integers that fall in the given range.
   * The array may contain duplicates.
   */
  def countInRange(xs: IndexedSeq[Int], lo: Int, hi: Int): Int = {
    @tailrec
    def find(target: Int, low: Int, high: Int, cmp: (Int, Int) => Boolean): Int = {
      if (high <= low) low
      else {
        val mid = low + (high - low) / 2

        if (cmp(xs(mid), target)) find(target, low, mid - 1, cmp)
        else find(target, mid + 1, high, cmp)
      }
    }

    val n = xs.length - 1
    val left = find(lo, 0, n, Ordering[Int].gteq)
    val right = find(hi, 0, n, Ordering[Int].gt)

    right - left
  }

  /*
   * An sorted array of integers was rotated an unknown number of times.
   * Given such an array, find the index of the element in the array in faster than linear time. If the element doesn't
   * exist in the array, return null.
   *
   * For example, given the array [13, 18, 25, 2, 8, 10] and the element 8, return 4 (the index of 8 in the array).
   * You can assume all the integers in the array are unique.
   */
  def searchInSortedRotatedArray(xs: IndexedSeq[Int], x: Int): Int = {
    @tailrec
    def search(lo: Int, hi: Int): Int = {
      if (lo > hi) -1
      else {
        val mid = lo + (hi - lo) / 2
        if (xs(mid) == x) mid
        else if (x < xs(lo) || x > xs(mid)) search(mid + 1, hi)
        else search(lo, mid - 1)
      }
    }

    search(0, xs.size - 1)
  }
}

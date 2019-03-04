package org.asarkar.codinginterview

package object dp {
  /*
   * Recursive staircase problem
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/dp/package.scala
   */

  /*
   * Given an array A of distinct positive integers, and an integer k, how many subsets of the array sum to k?
   *
   * ANSWER: Consider the last element of A; it may or may not be included in a subset that sums to k. If it's included,
   * then the number of subsets that sum to k is equal to the number of subsets of A - {A[i]} that sum to k - A[i].
   * If it's not included, then the number of subsets that sum to k is equal to the number of subsets of A - {A[i]}
   * that sum to k. Note that if the last element is greater than the sum k, it can't be included in any subset that
   * sums to k, and the first case doesn't apply.
   * Let dp[i][j] be the number of subsets that sum to j with or without the i-th element. When i = 0, no elements are
   * included, and no subsets are possible. When j = 0, only one set can add up to the empty set
   * (since all elements are positive), the empty set.
   */
  def numSubsetsOfSumK(xs: IndexedSeq[Int], k: Int): Int = {
    val dp = Array.tabulate[Int](xs.size + 1, k + 1)((_, j) => if (j == 0) 1 else 0)

    for (i <- 1 to xs.size; j <- 1 to k) {
      dp(i)(j) = dp(i - 1)(j) + (if (j >= xs(i - 1)) dp(i - 1)(j - xs(i - 1)) else 0)
    }

    dp(xs.size)(k)
  }

  /*
   * Best Time to Buy and Sell Stock, if you're allowed n transactions
   *
   * ANSWER:
   * https://www.youtube.com/watch?v=oDhu5uGq_ic
   *
   * Let dp(i)(j) is the max profit made by making i transactions till the j-th day.
   * On the j-th day, we have two choices:
   * 1. We make no transactions, i.e. neither sell not buy any stocks. In this case, the max profit is the same as
   *    what was on the previous (j - 1) day.
   * 2. We sell the stock we're holding, and buy a new one. In this case, the max profit is the difference of the
   *    selling price on the j-th day and the buying price on the day the stock was bought, plus the profit that was
   *    on the j-th day before buying the stock. Since we don't know which day the stock was bought, we will try all
   *    days until the previous day (j - 1).
   *
   * Thus, dp(i)(j) = max { dp(i)(j - 1), prices(j) - prices(k) + dp(i - 1)(k) }, k = 0 to j - 1
   *
   * This algorithm works, but has a time complexity of  O(|prices| x n²). The squared term comes from the fact that
   * for each day, we are iterating over all values of days until the current one.
   *
   * Can we do better? To answer that, let's look at the second term more closely.
   * prices(j) + (dp(i - 1)(k) - prices(k))
   *
   * prices(j) is constant for a given j. At the end of each iteration of j, if we compare the maximum value for
   * dp(i - 1)(k) - prices(k) with dp(i - 1)(j) - prices(j), and store it for the next iteration,
   * we don't have to start over from j = 0 every time. That brings us to the following improved formula:
   *
   * dp(i)(j) = max { dp(i)(j - 1), prices(j) + max { dp(i - 1)(j) - prices(j), maxDiff } }
   *
   * Initially, since we have no previous profit, and no stock to sell, maxDiff is negative value of the first price,
   * since buying the stock incurs cost that we have not recovered yet. We reset this value whenever we start from day
   * one, i.e. for every i.
   *
   * Now, calculating the second term takes only couple of max operations(O(1)); thus, overall time complexity is
   * O(|prices| x n).
   */
  def stocks3(prices: IndexedSeq[Int], n: Int): Int = {
    val dp = Array.ofDim[Int](n + 1, prices.size)

    for (i <- 1 to n) {
      var maxDiff = -prices(0)
      for (j <- 1 until prices.size) {
        dp(i)(j) = math.max(dp(i)(j - 1), prices(j) + maxDiff)
        maxDiff = math.max(dp(i - 1)(j) - prices(j), maxDiff)
      }
    }

    dp(n)(prices.size - 1)
  }

  /*
   * Given a list of integers, write a function that returns the largest sum of non-adjacent numbers.
   * Numbers can be 0 or negative.
   * Follow-up: Can you do this in O(N) time and constant space?
   *
   * ANSWER: Let dp(i) denote the maximum sum up to the i-th element. If the i-th element is included in the max sum,
   * dp(i) is obtained by adding it with the max sum obtained so far up to the (i - 2)-th element. If the i-th element
   * isn't included in the max sum, dp(i) = dp(i - 1). Observe that it's not sufficient to consider the sum of the
   * i-th element and the (i - 2)-th element. For example, given array [5, 1, 1, 5], the max sum 10 is obtained by the
   * sum of the 1st and the last elements, not the sum of the 1st and 3rd or the 2nd and the last elements.
   *
   * We could iterate over all values of j in [0, i - 2] for each i, but that would give us an O(n^2) algorithm. Instead,
   * for each i, we keep track of the max value until i - 2. That gives us a O(n) algorithm. Also note that, in order to
   * calculate dp(i), we only need the max value until i - 2, and dp(i - 1). Thus, we can solve this problem in O(2)
   * space, or constant space.
   */
  def largestNonAdjacentSum(xs: IndexedSeq[Int]): Int = {
    val dp = Array.tabulate[Int](2)(xs)

    (2 until xs.size).foldLeft(dp.head) { (max, i) =>
      val j = i % 2
      dp(j) = math.max(max + xs(i), dp(j ^ 1))
      math.max(max, dp(j ^ 1))
    }

    dp.max
  }
}

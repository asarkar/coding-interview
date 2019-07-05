package org.asarkar.codinginterview

import java.util.regex.PatternSyntaxException

import scala.annotation.tailrec

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

  /*
   * A builder is looking to build a row of N houses that can be of K different colors. He has a goal of minimizing
   * cost while ensuring that no two neighboring houses are of the same color.
   * Given an N by K matrix where the nth row and kth column represents the cost to build the nth house with kth color,
   * return the minimum cost which achieves this goal.
   *
   * ANSWER: Let dp[i][j] represent the cost of coloring up to the i-th house, with the i-th house painted with the
   * j-th color.
   * dp[i][j] = cost[i][j] + min{dp[i - 1][x]}, j, x in [0, K), j != x
   *
   * Row dp[N - 1] represents the costs of coloring all the houses such that no two neighboring houses are of the same
   * color. We simply need to find the minimum value in this row.
   *
   * Time complexity: For each i, we go over K j's, and for each (i, j), we go over K costs. Time complexity: O(NK^2).
   * Space complexity: O(NK). However, we only need the last row, so we can save space by discarding the rest.
   */
  def minCostOfColoringHouses(costs: IndexedSeq[IndexedSeq[Int]]): Int = {
    val dp = Array.tabulate[Int](costs.size, costs.head.size)((i, j) => costs(i)(j))

    for (i <- 1 until costs.size; j <- costs(i).indices) {
      dp(i)(j) += costs(i)
        .indices
        .filterNot(_ == j)
        .map(dp(i - 1)(_))
        .min
    }

    dp.last.min
  }

  /*
   * Implement regular expression matching with the following special characters:
   *   . (period) which matches any single character
   *   * (asterisk) which matches zero or more of the preceding element
   *
   * That is, implement a function that takes in a string and a valid regular expression and returns whether or not the
   * string matches the regular expression.
   *
   * For example, given the regular expression "ra." and the string "ray", your function should return true. The same
   * regular expression on the string "raymond" should return false.
   * Given the regular expression ".*at" and the string "chat", your function should return true. The same regular
   * expression on the string "chats" should return false.
   *
   * ANSWER: We implement a DP solution. See https://www.youtube.com/watch?v=l3hda49XcDE
   * It also checks that there are no dangling wildcards (in other words, a wildcard must be preceded by a character).
   * Time and space complexity: O(mn).
   *
   * The classic approach is to convert the regex to a NFA, which takes same time but less space (O(n) in the worst
   * case, if the state machine can be in all of the possible states).
   * See https://blog.asarkar.org/coding-interview-curated/#system-design
   *
   * However, we can achieve O(n) space with this solution too if we store only the last two rows.
   */
  implicit class Metachar(val ch: Char) extends AnyVal {
    def isAnyChar: Boolean = ch == '.'

    def isWildcard: Boolean = ch == '*'
  }

  def isRegexMatch(pattern: String, text: String): Boolean = {
    if (pattern.headOption.getOrElse('\u0000').isWildcard)
      throw new PatternSyntaxException("Dangling meta character '*'", pattern, 0)

    val m = pattern.length
    val n = text.length

    // dp[i][j] indicates the match status of the pattern prefix with i char, and the text prefix with j char
    val dp = Array.ofDim[Boolean](m + 1, n + 1)

    /*
     * two patterns match empty text:
     * - empty pattern.
     * - alternating wildcards (pattern a*b* matches empty text).
     */
    for (row <- 0 to m by 2 if row == 0 || pattern(row - 1).isWildcard) {
      dp(row)(0) = (row == 0) || dp(row - 2)(0)
    }

    for (row <- 1 to m; col <- 1 to n) {
      val patternCh = pattern(row - 1)
      val textCh = text(col - 1)

      dp(row)(col) = if (patternCh == textCh || patternCh.isAnyChar)
      // current characters match, match the rest
        dp(row - 1)(col - 1)
      else
      /*
       * if current char in the pattern is a wildcard, there can be a match if one of the following is true:
       * - there are zero occurrences of the preceding char in the pattern in at this position of the text,
       * in which case, the pattern with two char removed (the wildcard and the preceding char) must match the
       * text so far. for example, the pattern "ab*" matches the text "a".
       * - there is at least one occurrences of the preceding char in the pattern at this position of the text,
       * in which case, the preceding char in the pattern must match the current char in the text, and the pattern
       * must match the text with the current character removed. for example, the pattern "ab*" matches the text
       * "abb".
       */
        patternCh.isWildcard &&
          (dp(row - 2)(col) ||
            (dp(row)(col - 1) && (pattern(row - 2) == textCh || pattern(row - 2).isAnyChar)))
    }

    dp(m)(n)
  }

  /*
   * Write an algorithm to justify text. Given a sequence of words and an integer line length k, return a list of
   * strings which represents each line, fully justified.
   * More specifically, you should have as many words as possible in each line. There should be at least one space
   * between each word. Pad extra spaces when necessary so that each line has exactly length k. Spaces should be
   * distributed as equally as possible, with the extra spaces, if any, distributed starting from the left.
   * If you can only fit one word on a line, then you should pad the right-hand side with spaces.
   * Each word is guaranteed not to be longer than k.
   *
   * For example, given the list of words ["the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"]
   * and k = 16, you should return the following:
   * ["the  quick brown", # 1 extra space on the left
   * "fox  jumps  over", # 2 extra spaces distributed evenly
   * "the   lazy   dog"] # 4 extra spaces distributed evenly
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/dp/package.scala
   */

  /*
   * The edit distance between two strings refers to the minimum number of character insertions, deletions, and
   * substitutions required to change one string to the other. For example, the edit distance between "kitten" and
   * "sitting" is three: substitute the 'k' for 's', substitute the 'e' for 'i', and append a 'g'.
   *
   * Given two strings, compute the edit distance between them.
   *
   * ANSWERS: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/dp/package.scala
   */

  /*
   * Given a string, find the palindrome that can be made by inserting the fewest number of characters as possible
   * anywhere in the word. If there is more than one palindrome of minimum length that can be made, return the
   * lexicographically earliest one (the first one alphabetically).
   * For example, given the string "race", you should return "ecarace", since we can add three letters to it
   * (which is the smallest amount to make a palindrome). There are seven other palindromes that can be made from
   * "race" by adding three letters, but "ecarace" comes first alphabetically.
   *
   * As another example, given the string "google", you should return "elgoogle".
   *
   * ANSWER: Observe that for any string s[i..j], if s[i] == s[j], then the number of insertions required to make it
   * a palindrome is the same as the number of insertions required to make s[i+1..j-1] a palindrome.
   * If, however, s[i] != s[j], then we may convert s[i..j-1] to a palindrome and then insert s[j] at the beginning, or
   * convert s[i+1..j] to a palindrome and insert s[i] at the end. Since we are looking for the fewest number of
   * insertions, we will choose the minimum of the two options. The number of insertions is one more than the number
   * of insertions required for the chosen subproblem (for adding a character at the beginning or at the end).
   * If number of insertions are equal, we choose the one corresponding to the lexicographically smaller character.
   *
   * Let dp[i][j] store the number of insertions required to convert s[i..j] to a palindrome. For subproblems of size 1,
   * no insertions are required. We start with subproblems of size two (s[i..j], i and j both inclusive), and fill up
   * the matrix incrementally.
   */
  def makePalindromeByFewestEdits(word: String): String = {
    val n = word.length
    val dp = Array.ofDim[Int](n, n)

    for (window <- 1 until n)
      (0 until n)
        .map(start => (start, start + window))
        .takeWhile(_._2 < n)
        .foreach {
          case (start, end) if word(start) == word(end) =>
            dp(start)(end) = dp(start + 1)(end - 1)
          case (start, end) =>
            dp(start)(end) = math.min(dp(start + 1)(end), dp(start)(end - 1)) + 1
        }

    val minInsertions = dp(0)(n - 1)
    val palindrome = Array.ofDim[Char](n + minInsertions)

    @tailrec
    def reconstruct(start: Int, end: Int, count: Int, offset: Int): String = {
      if (count == 0) {
        // we have written 'start' characters from the beginning, the current insertion index is 'offset', and
        // the number of characters left to be written are the substring word[start..end]
        Array.copy(word.toCharArray, start, palindrome, offset, end - start + 1)
        palindrome.mkString
      } else {
        val (s, e, c, ch) = if (word(start) == word(end))
          (start + 1, end - 1, count, word(start))
        else if (dp(start + 1)(end) < dp(start)(end - 1) ||
          (dp(start + 1)(end) == dp(start)(end - 1) && word(start) < word(end))
        )
          (start + 1, end, count - 1, word(start))
        else
          (start, end - 1, count - 1, word(end))

        palindrome(offset) = ch
        palindrome(palindrome.length - 1 - offset) = ch
        reconstruct(s, e, c, offset + 1)
      }
    }

    reconstruct(0, n - 1, minInsertions, 0)
  }

  /*
   * Given a list of integers S and a target number k, write a function that returns a subset of S that adds up to k.
   * If such a subset cannot be made, then return null.
   *
   * Integers can appear more than once in the list. You may assume all numbers in the list are positive.
   *
   * For example, given S = [12, 1, 61, 5, 9, 2] and k = 24, return [12, 9, 2, 1] since it sums up to 24.
   *
   * ANSWER: This problem exhibits optimal substructure, and overlapping problems, so we will use dynamic programming
   * to solve it.
   *
   * Optimal substructure: If the integer in the i-th position (a[i - 1]) is included in an optional solution, then
   * any subset up to, and possibly including, a[i - 2] must add up to j - a[i - 1].
   * On the other hand, there are two cases where a[i - 1] can't be included in an optional solution:
   * - If it's greater than j (obviously).
   * - Even if it's smaller than j, it's possible that the some subset of the first i - 1 integers adds up to j already.
   *
   * Let dp[i][j] be true if some subset of the first i integers exactly adds up to j, false otherwise.
   * Since the empty set is a subset of all such subsets, d[0][0] is true. The following code follows naturally.
   */
  def subsetSum(xs: IndexedSeq[Int], k: Int): Seq[Int] = {
    val dp = Array.tabulate[Boolean](xs.size + 1, k + 1) { (i, j) => i == 0 && j == 0 }

    for (i <- 1 to xs.size; j <- 1 to k) {
      val cur = xs(i - 1)

      dp(i)(j) =
        if (cur > j) dp(i - 1)(j)
        else dp(i - 1)(j) || dp(i - 1)(j - cur)
    }

    Iterator
      .iterate((xs.size, k, -1)) {
        case (i, j, _) =>
          if (i == 0 || dp(i)(j) == dp(i - 1)(j)) (i - 1, j, -1) // xs(i - 1) not included
          else (i - 1, j - xs(i - 1), xs(i - 1)) // xs(i - 1) included
      }
      .takeWhile {
        case (i, j, _) => dp.lift(i).exists(_.exists(y => y))
      }
      .filter(_._3 > 0)
      .map(_._3)
      .toSeq
  }

  /*
   * Given an array of numbers, find the maximum sum of any contiguous subarray of the array.
   *
   * For example, given the array [34, -50, 42, 14, -5, 86], the maximum sum would be 137, since we would take elements
   * 42, 14, -5, and 86.
   * Given the array [-5, -1, -8, -9], the maximum sum would be 0, since we would not take any elements.
   *
   * Do this in O(N) time.
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/dp/package.scala
   */

  /*
   * Given a string s and an integer k, break up the string into multiple lines such that each line has a length of k
   * or less. You must break it up so that words don't break across lines. Each line has to have the maximum possible
   * amount of words. If there's no way to break the text up, then return null.
   *
   * You can assume that there are no spaces at the ends of the string and that there is exactly one space between each
   * word.
   *
   * For example, given the string "the quick brown fox jumps over the lazy dog" and k = 10, you should return:
   * ["the quick", "brown fox", "jumps over", "the lazy", "dog"]. No string in the list has a length of more than 10.
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/dp/package.scala
   */

  /*
   * Given a multiset of integers, return whether it can be partitioned into two subsets whose sums are the same.
   *
   * For example, given the multiset {15, 5, 20, 10, 35, 15, 10}, it would return true, since we can split it up into
   * {15, 5, 10, 15, 10} and {20, 35}, which both add up to 55.
   *
   * Given the multiset {15, 5, 20, 10, 35}, it would return false, since we can't split it up into two subsets that
   * add up to the same sum.
   *
   * ANSWER: We simply find a subset that adds up to k/2. See subsetSum.
   */

  /*
   * There is an N by M matrix of zeroes. Given N and M, write a function to count the number of ways of starting at
   * the top-left corner and getting to the bottom-right corner. You can only move right or down.
   * For example, given a 2 by 2 matrix, you should return 2, since there are two ways to get to the bottom-right:
   * Right, then down
   * Down, then right
   *
   * Given a 5 by 5 matrix, there are 70 ways to get to the bottom-right.
   *
   * ANSWER: We solve this by dynamic programming. Let dp[i][j] be the number of ways to reach cell (i, j) from the
   * top-left corner (cell (0, 0)). dp[i][j] is simply the sum of the number of ways to reach its neighbors.
   *
   * dp[0][0] = 1, since the only way to get to cell (0, 0) from itself is by not going anywhere.
   */
  def numWays(m: Int, n: Int): Int = {
    val dp = Array.ofDim[Int](m, n)
    dp(0)(0) = 1

    def neighbors(row: Int, col: Int): Seq[(Int, Int)] = {
      Seq((row - 1, col), (row, col - 1))
        .filter { case (r, c) => r >= 0 && r < m && c >= 0 && c < n }
    }

    for (row <- 0 until m; col <- 0 until n) {
      if (row + col > 0) {
        dp(row)(col) = neighbors(row, col)
          .map(x => dp(x._1)(x._2))
          .sum
      }
    }

    dp(m - 1)(n - 1)
  }

  /*
   * Given n, how many structurally unique BST's (binary search trees) that store values 1 ... n?
   *
   * Example:
   * Input: 3
   * Output: 5
   * Explanation:
   * Given n = 3, there are a total of 5 unique BST's:
   * 1         3     3      2      1
   *  \       /     /      / \      \
   *   3     2     1      1   3      2
   *  /     /       \                 \
   * 2     1         2                 3
   *
   * ANSWER: The number of unique BSTs is product of the number of unique BSTs of the left range and the right range.
   * That's because every one of the BSTs on the left can be combined with every one of the BSTs on the right to create
   * a unique BST rooted at the same node.
   *
   * Since the problem has overlapping subproblems, we memoize the results.
   *
   * Time complexity: For every value in 1..n, we call numTrees recursively that only rewinds when lo = hi. Thus, for
   * n = 1, there's hi - 1 calls, n = 2, hi - 1 calls, and so on. However, due to the bottom up DP, only the first
   * recursive calls do work, others simply return the memoized values. Thus, time complexity is O(n).
   */
  def numUniqBST(n: Int): Int = {
    val dp = Array.ofDim[Int](n + 1, n + 1)

    def numTrees(lo: Int, hi: Int): Int = {
      if (lo > hi) 0
      else if (dp(lo)(hi) > 0) dp(lo)(hi)
      else {
        val total = (lo to hi)
          .map(i => math.max(1, numTrees(lo, i - 1)) * math.max(1, numTrees(i + 1, hi)))
          .sum
        dp(lo)(hi) = total
        total
      }
    }

    numTrees(1, n)
  }

  /*
   * How many ways to decode this message? For example, given encoding a = 1, b = 2, c = 3, ..., z = 26, what does the
   * encoded string '12' represent? It could be 'ab' or 'l'. For simplicity, assume that the encoded string contains
   * only digits 0-9. Can you solve this in O(n) time, where n is the length of the encoded string?
   *
   * ANSWER: Observe that the number of ways is simply the number of leaves in the recursion tree. If we represent
   * the number of ways to decode a message of length n to be N(n), observe that N(n) = N(n - 1) + N(n - 2),
   * where the second term is present only if the first two characters converted to a digit is smaller than the maximum
   * digit in the encoding. For the given question, the maximum digit is 26 corresponding to the letter 'z'. Similarly,
   * if the message starts with a digit smaller than the minimum digit in the encoding (1, corresponding to 'a'), there
   * is no way to decode it.
   *
   * To avoid creating new strings, we introduce 'i' as the number of characters to consider from the right. Since the
   * subproblems are overlapping (notice the recurrence above, it looks very much like Fibonacci), we also memoize the
   * intermediate results to avoid recalculations.
   */
  def numWaysToDecode(msg: String): Int = {
    // dp(i) is the number of ways to decode the last i char of the message
    val dp = Array.fill[Int](msg.length + 1)(-1)
    val (lowest, highest) = (1, 26)

    def loop(i: Int): Int = {
      if (i == 0) 1
      else {
        // start index of the substring we are examining (i.e. msg[j..i))
        val j = msg.length - i

        if (msg(j).asDigit < lowest) 0
        else if (dp(i) >= 0) dp(i)
        else {
          dp(i) = loop(i - 1) +
            (if (msg.isDefinedAt(j + 1) && msg.substring(j, j + 2).toInt <= highest) loop(i - 2)
            else 0)

          dp(i)
        }
      }
    }

    loop(msg.length)
  }

  /*
   * Given a string which we can delete at most k, return whether you can make a palindrome.
   * For example, given 'waterrfetawx' and a k of 2, you could delete f and x to get 'waterretaw'.
   *
   * ANSWER: This is a special case of the edit distance problem, where we check if a string s can be converted to a
   * string p by only deleting characters from either or both strings.
   *
   * This problem exhibits optimal substructure (if a string is a k-palindrome, some substrings are also k-palindromes),
   * and overlapping subproblems (the solution requires comparing the same substrings more than once).
   *
   * Let the string be s and its reverse rev. Let dp[i][j] be the number of deletions required to convert the first i
   * characters of s to the first j characters of rev. Since deletions have to be done in both strings,
   * if dp[n][n] <= 2 * k, then the string is a k-palindrome.
   *
   * Base case: When one of the strings is empty, all characters from the other string need to be deleted in order
   * to make them equal.
   *
   * Time complexity: O(n^2).
   *
   * Since we are doing bottom-up DP, an optimization is to return false if at any time i == j && dp[i][j] > 2 * k,
   * since all subsequent i == j must be greater.
   */
  def kPalindrome(s: String, k: Int): Boolean = {
    val rev = s.reverse
    val n = s.length
    val dp = Array.ofDim[Int](n + 1, n + 1)

    for (i <- 0 to n; j <- 0 to n) {
      dp(i)(j) = if (i == 0 || j == 0) i + j
      else if (s(i - 1) == rev(j - 1)) dp(i - 1)(j - 1)
      else 1 + math.min(dp(i - 1)(j), dp(i)(j - 1))
    }
    dp(n)(n) <= 2 * k
  }

  /*
   * You are given a 2-d matrix where each cell represents number of coins in that cell. Assuming we start at
   * matrix[0][0], and can only move right or down, find the maximum number of coins you can collect by the bottom
   * right corner.
   *
   * For example, in this matrix
   * 0 3 1 1
   * 2 0 0 4
   * 1 5 3 1
   *
   * The most we can collect is 0 + 2 + 1 + 5 + 3 + 1 = 12 coins.
   *
   * ANSWER: Top-down DP.
   */
  def maxCoins(matrix: Seq[Seq[Int]]): Int = {
    def neighbors(row: Int, col: Int): Set[(Int, Int)] = {
      Set((row, col - 1), (row - 1, col))
        .filter(x => matrix.isDefinedAt(x._1) && matrix(x._1).isDefinedAt(x._2))
    }

    val dp = Array.tabulate[Int](matrix.size, matrix.last.size)((row, col) => if (row == 0 && col == 0) 0 else -1)

    def loop(row: Int, col: Int): Int = {
      if (dp(row)(col) < 0) {
        dp(row)(col) = matrix(row)(col) + neighbors(row, col).foldLeft(0) { case (acc, (r, c)) =>
          math.max(acc, loop(r, c))
        }
      }
      dp(row)(col)
    }

    loop(matrix.size - 1, matrix.last.size - 1)
  }
}

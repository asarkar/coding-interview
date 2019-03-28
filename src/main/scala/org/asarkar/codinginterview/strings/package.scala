package org.asarkar.codinginterview

import java.util.Locale

import scala.annotation.tailrec

package object strings {
  /*
   * Longest consecutive characters
   */
  def longestConsecutive(s: String): (Char, Int) = {
    Iterator.iterate(('\u0000', 0, 0)) { case (ch, longestRun, i) =>
      val run = (i until s.length)
        .takeWhile(s(_) == s(i))
        .size

      if (run > longestRun) (s(i), run, i + run)
      else (ch, longestRun, i + run)
    }
      .dropWhile(i => s.isDefinedAt(i._3))
      .take(1)
      .map(x => (x._1, x._2))
      .next()
  }

  /*
   * Given a string, find the first non-repeating character in it
   *
   * ANSWER: This takes two passes, first through the string, and then through the map. Time complexity: O(n).
   */
  def firstNonRepeatingChar(s: String): (Char, Int) = {
    s.zipWithIndex
      // group by each character
      .groupBy(_._1)
      // keep only those entries that appear exactly once
      .filter(_._2.size == 1)
      // map to the indices
      .mapValues(_.head._2)
      // find the min index
      .minBy(_._2)
  }

  /*
   * Check if edit distance between two strings is one
   *
   * ANSWER: The general solution is to calculate the edit distance between the two strings and check if it is equal to
   * one. See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/dp/package.scala
   * However, that is a O(n²) solution, and we can solve this problem as a special case in O(n).
   *
   * We run two pointers, i and j, and maintain a count of edit distance so far. While we haven't exhausted any of the
   * strings, and count is less than two, do the following:
   * 1. If s1[i] == s2[j], no edit required, increment both pointers.
   * 2. Otherwise, if the strings are of equal length, then one character can be replaced by the other. Increment
   * counter, and advance both pointers.
   * 3. Otherwise, the character in the longer string can be deleted. Increment counter, and advance the pointer
   * corresponding to the longer string.
   *
   * In the end, increment counter if one of the strings has been exhausted but the other one hasn't.
   */
  def isOneAway(s1: String, s2: String): Boolean = {
    val m = s1.length
    val n = s2.length

    if (math.abs(m - n) > 1) false
    else {
      Iterator.iterate((0, 0, 0)) { case (i, j, count) =>
        if (s1(i) == s2(j)) (i + 1, j + 1, count)
        else if (m > n) (i + 1, j, count + 1)
        else if (n > m) (i, j + 1, count + 1)
        else (i + 1, j + 1, count + 1)
      }
        .dropWhile(x => s1.isDefinedAt(x._1) && s2.isDefinedAt(x._2) && x._3 <= 1)
        .take(1)
        .map(x => x._3 + (if (s1.isDefinedAt(x._1) || s2.isDefinedAt(x._2)) 1 else 0))
        .next() == 1
    }
  }

  /*
   * Test if a given string is a palindrome. Ignore any characters that's not a letter. For example, "Borrow or rob?"
   * should be considered as a palindrome.
   *
   * ANSWER: Maintain two indices, i and j, starting from the beginning and the end of the string, respectively.
   * At each iteration, check if the characters at positions i and j are the same; if yes, continue by incrementing i
   * and decrementing j; if not, break the loop. If i == j in the end, then the string is a palindrome.
   * Time complexity: O(n), space complexity: O(1).
   */
  def isPalindrome(s: String): Boolean = {
    Iterator.iterate((0, s.length - 1)) { case (i, j) =>
      if (s(i).isLetter && !s(j).isLetter) (i, j - 1)
      else if (!s(i).isLetter && s(j).isLetter) (i + 1, j)
      else if (s(i) == s(j)) (i + 1, j - 1)
      else (Int.MaxValue, j)
    }
      .dropWhile(x => x._1 < x._2)
      .take(1)
      // >= takes care of the empty string; = wouldn't
      .map(x => x._1 >= x._2)
      .next()
  }

  /*
   * Reverse a string
   */
  def reverse(s: String): String = {
    @tailrec
    def loop(front: Int, rear: Int, chars: Array[Char]): String = {
      if (front < rear) {
        val ch = chars(front)
        chars(front) = chars(rear)
        chars(rear) = ch

        loop(front + 1, rear - 1, chars)
      } else chars.mkString
    }

    loop(0, s.length - 1, s.toCharArray)
  }

  /*
   * Convert a string to decimal
   *
   * ANSWER: See https://github.com/asarkar/epi/blob/master/src/main/scala/org/asarkar/epi/strings/package.scala
   *
   * Handle leading zeros ("000123"), negative numbers, and invalid input ("A"). Also ask the interview what to do if
   * the input is too long or too small for an int.
   */

  /*
   * Given a sequence of words, print all anagrams together
   *
   * ANSWER: We sort each word, and group them by hash code. All groups having more than one words go in the result.
   * Sorting takes O(m log(m)) if the average word length is m. If the input contains n words, total time complexity
   * is O(mn log(m)).
   * We can improve on this by using counting sort instead of a comparison based sort. For simplicity,
   * we only consider lowercase word characters in a string for sorting and hashing. Time complexity: O(mn).
   *
   * Variant: Anagram substring search
   * Given text "BACDGABCDA", pattern "ABCD" is found at indices 0, 5 and 6.
   * Can be solved by generating all subsets of length 4 ("ABCD".length), and then checking if they are anagrams of
   * "ABCD".
   */
  def anagrams1(words: Seq[String]): Seq[Set[String]] = {
    words
      .map(w => (countingSort(w.toLowerCase(Locale.ENGLISH).replaceAll("\\W", "")).hashCode, w))
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)
      .filter(_._2.size > 1)
      .values
      .toSeq
  }

  // https://www.youtube.com/watch?v=OKd534EWcdk
  def countingSort(word: String): String = {
    val xs = Array.ofDim[Int](26)

    word
      .foreach(c => xs(c - 'a') += 1)

    (1 until xs.length)
      .foreach(i => xs(i) += xs(i - 1))

    (xs.length - 1 to 1 by -1)
      .foreach(i => xs(i) = xs(i - 1))
    xs(0) = 0

    word
      .foldLeft(Array.ofDim[Char](word.length)) { (buffer, c) =>
        buffer(xs(c - 'a')) = c
        xs(c - 'a') += 1
        buffer
      }
      .mkString
  }

  /*
   * Given an integer k and a string s, find the length of the longest substring that contains at most k distinct
   * characters.
   * For example, given s = "abcba" and k = 2, the longest substring with k distinct characters is "bcb".
   *
   * ANSWER: A brute force solution would be to find all substrings of the given string, and then count the frequency
   * of distinct characters in each one. To derive the number of substrings of a string, we observe that:
   * Number of substrings of length one is n (We can choose any of the n characters).
   * Number of substrings of length two is n-1 (We can choose any of the n-1 pairs formed by adjacent characters).
   * Number of substrings of length three is n-2 (We can choose any of the n-2 triplets formed by adjacent characters).
   *
   * In general, number of substrings of length k is n-k+1 where 1 <= k <= n.
   * Total number of substrings of all lengths from 1 to n is given by:
   * n + (n-1) + (n-2) + (n-3) + ... + 2 + 1
   * = n * (n + 1)/2
   * = O(n^2)
   *
   * If we keep track of the number of distinct characters in each substring while building it, we get a O(n^2)
   * solution. Can we do better?
   *
   * We can! The idea is maintain a sliding window that expands on the right until the number of distinct characters in
   * the window exceeds k. At that point, we shrink the window from the left until the the number of distinct characters
   * in the window is less than or equal to k, and then continue expanding on the right.
   * The algorithm terminates when the left boundary touches the right (only possible for k > 1), or the right boundary
   * exceeds the length of the string.
   */
  def longestSubstringWithKDistinctChar(str: String, k: Int): Int = {
    val charFreq = collection.mutable.Map.empty[Char, Int]

    @tailrec
    def loop(start: Int, end: Int, longestWindow: (Int, Int), numDistinctChars: Int): (Int, Int) = {
      if (str.isDefinedAt(end) && start <= end) {
        val chAtEnd = str(end)
        val count = charFreq.getOrElse(chAtEnd, 0) + 1
        val x = numDistinctChars + (if (count == 1) 1 else 0)

        if (x <= k) {
          charFreq(chAtEnd) = count
          val currentWindowSize = end - start + 1
          val longestWindowSize = longestWindow._2 - longestWindow._1 + 1
          val newLongestWindow = if (currentWindowSize > longestWindowSize) (start, end + 1) else longestWindow
          loop(start, end + 1, newLongestWindow, x)
        } else {
          val chAtStart = str(start)
          charFreq(chAtStart) -= 1
          val y = numDistinctChars - (if (charFreq(chAtStart) == 0) 1 else 0)
          loop(start + 1, end, longestWindow, y)
        }
      } else longestWindow
    }

    val (start, end) = loop(0, 0, (-1, -1), 0)
    val substr = str.slice(start, end + 1)
    println(s"str = $str, k = $k, longest substring = $substr")
    substr.length
  }

  /*
   * Run-length encoding is a fast and simple method of encoding strings. The basic idea is to represent repeated
   * successive characters as a single count and character. For example, the string "AAAABBBCCDAA" would be encoded
   * as "4A3B2C1D2A".
   * Implement run-length encoding and decoding. You can assume the string to be encoded have no digits and consists
   * solely of alphabetic characters. You can assume the string to be decoded is valid.
   */
  def runLengthEncoding(str: String): String = {
    (1 to str.length)
      .foldLeft((1, new StringBuilder())) { case ((count, buffer), i) =>
        if (str.lift(i).contains(str(i - 1))) (count + 1, buffer)
        else (1, buffer.append(count).append(str(i - 1)))
      }
      ._2
      .mkString
  }

  def runLengthDecoding(str: String): String = {
    str
      .sliding(2, 2)
      .foldLeft(new StringBuilder())((buffer, pair) =>
        (1 to pair.head.asDigit)
          .foldLeft(buffer)((x, _) => x.append(pair.last))
      )
      .toString()
  }

  /*
   * Given two strings - input1 and input2, determine if they are isomorphic.
   * Two strings are isomorphic if the letters in one string can be remapped to get the second string. Remapping a
   * letter means replacing all occurrences of it with another letter. The ordering of the letters remains unchanged.
   * You can also think of isomorphism as it is used in chemistry - i.e. having the same form or overall shape.
   * Target linear time and space complexity with your solution.
   *
   * ANSWER: What the question doesn't say is the following:
   * - same character in s1 can't be mapped to different characters in s2 ("aba" and "baa" are not isomorphic)
   * - different characters in s1 can't be mapped to the same character in s2 ("abcd" and "aabb" are not isomorphic)
   *
   * To check for the above conditions, we maintain two maps, chars of s1 to chars of s2, and chars of s2 to chars of
   * s1. The rest of the algorithm is pretty straightforward.
   */
  def isIsomorphic(s1: String, s2: String): Boolean = {
    if (s1.length != s2.length) false
    else Iterator.iterate((Map.empty[Char, Char], Map.empty[Char, Char], 0, true)) { case (map1, map2, i, _) =>
      val ch1 = s1(i)
      val ch2 = s2(i)
      if (map1.getOrElse(ch1, ch2) != ch2 || map2.getOrElse(ch2, ch1) != ch1) (map1, map2, i + 1, false)
      else (map1 + (ch1 -> ch2), map2 + (ch2 -> ch1), i + 1, true)
    }
      .dropWhile(x => (x._3 < s1.length) && x._4)
      .take(1)
      .map(_._4)
      .next()
  }

  /*
   * Given a string, find the longest palindromic contiguous substring. If there are more than one with the maximum
   * length, return any one.
   *
   * For example, the longest palindromic substring of "aabcdcb" is "bcdcb'. The longest palindromic substring of
   * "bananas" is "anana".
   *
   * ANSWER: The trivial algorithm for this is to consider each character as the center of a potential palindrome, and
   * expand around it. By expansion, we mean comparing the characters c - i and c + i for i > 0. This approach works,
   * but may take O(n^2) time in the worst case. To see why, consider the string "aaaaa". We do zero comparison for the
   * 1st char, one comparison for the second, and two for the third. Thus, we do (0 + 1 + ... + n/2) comparisons up to
   * the middle element, which adds up to (n/2 * (n/2 + 1)) / 2).
   * https://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_%E2%8B%AF
   *
   * Can we do better?
   *
   * We can, using Manacher's algorithm. The idea is to cut down the number of comparisons by utilizing the information
   * learned so far, and by making some cleaver insights. Let's introduce some notations first:
   * T: Given string transformed to insert a special character '#' in between every two characters, and also at the
   *    beginning and the end. This transforms every word to an odd-length word, thus allowing us to reason about the
   *    algorithm without having to worry about whether the given word is of odd-length or even. '#' is a random char
   *    chosen such that it already doesn't exist in the given string.
   * C: Center of the largest palindrome found so far.
   * R: Right edge of the palindrome centered at C; R >= C.
   * i: Some index > C.
   * P: Array that stores the length of the longest palindrome for 0 <= j < i.
   *
   * Since a palindrome is mirrored around its center, T[C - k] = T[C + k] for 0 <= k <= R - C. Let i' be the mirror of
   * i with respect to C, i.e. C - i' = i - C. What can we deduce about P[i] knowing P[i']?
   *
   * Case 1, P[i'] < R - C: The palindrome centered at i' is completely contained in the palindrome centered at C. In
   * this case, we can say that P[i] = P[i']. Why? Since there is a palindrome centered at C, T[i + k] = T[i' - k] for
   * all 0 <= k <= R - C. If P[i] could be larger than P[i'], we would have a contradiction. It would mean that we would
   * be able to extend the palindrome centered at i beyond P[i'], but if we could, then we would also be able to extend
   * the palindrome centered at i' due to the symmetry, but it was already supposed to be as large as possible.
   *
   * Case 2, P[i'] > R - C: P[i] = R - i. Why? Suppose P[i] were longer than this: that would imply that
   * T[R + 1] = T[i - (R - i + 1)]. T[i - (R - i + 1)] = T[i' + (R - i + 1)] because there's a palindrome centred at C;
   * and T[i' + (R - i + 1)] = T[i' - (R - i + 1)] because there's a palindrome of width at least R - i + 1 centred at
   * i' (since we have assumed P[i'] > R - i). i' - (R - i + 1) = L - 1, so what this means is that
   * T[R + 1] = T[L - 1]. But this is a contradiction since in that case, P[C] wouldn't be the largest.
   *
   * Case 3, P[i'] = R - C: We know P[i] to be at least at large at P[i'], but it can be larger, since we don't know
   * anything about the characters beyond R. Thus, we expand by launching the trivial algorithm beyond R.
   *
   * Case 2 and 3 can be coded as min(R - C, P[i']), and then by expanding. This doesn't increase the time complexity,
   * because in case 2, the expansion would fail immediately.
   *
   * Time Complexity is O(n), although I've not found a rigorous proof for this, and people seem to be hand waving the
   * analysis. The trivial algorithm takes linear time in the worst case, but how may times we launch the linear
   * algorithm is unclear.
   */
  def longestPalindromicSubstr(s: String): String = {
    val sb = s
      .foldLeft(new StringBuilder())((acc, c) => acc.append('#').append(c))
      .append('#')
    val n = sb.length - 1
    val dp = Array.ofDim[Int](n + 1)

    @tailrec
    def expand(center: Int, i: Int): String = {
      val right = center + dp(center)
      if (i > n || right == n) sb
        .slice(center - dp(center), right + 1)
        .filterNot(_ == '#')
        .toString()
      else {
        val distToRight = right - i
        val mirror = center - (i - center)
        // length of the longest palindrome centered at the mirror
        val x = dp.lift(mirror).getOrElse(0)

        dp(i) = if (x < distToRight) x
        else
          Iterator.from(math.min(x, distToRight))
            .dropWhile { j =>
              val (l, r) = (i - j, i + j)
              sb.isDefinedAt(l) && sb.isDefinedAt(r) && sb(l) == sb(r)
            }
            .take(1)
            .next() - 1 // we stop immediately after the last matching char

        if (dp(i) > dp(center)) expand(i, i + 1)
        else expand(center, i + 1)
      }
    }

    expand(0, 0)
  }
}

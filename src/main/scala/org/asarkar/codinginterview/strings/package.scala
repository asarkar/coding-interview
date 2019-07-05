package org.asarkar.codinginterview

import java.nio.charset.StandardCharsets
import java.util.Locale
import java.util.stream.Collectors

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

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

  /*
   * Implement a URL shortener with the following methods:
   * - shorten(url), which shortens the url into a six-character alphanumeric string, such as zLg6wl.
   * - restore(short), which expands the shortened string into the original url. If no such shortened string exists,
   *   return null.
   *
   * Hint: What if we enter the same URL twice?
   */
  def urlShortener1(url: String): String = {
    val bytes = Array.ofDim[Byte](6)
    new Random().nextBytes(bytes)

    new String(bytes, StandardCharsets.ISO_8859_1)
  }

  def urlShortener2(url: String): String = {
    val lo = 'a'.toInt
    val hi = 'z'.toInt
    new java.util.Random()
      // the hard way of doing this is (int)(Math.random() * ((max - min) + 1)) + min
      // Math.random() returns a random double between 0 (inclusive) and 1 (exclusive). Multiplying that by (max - min)
      // makes the range 0 (inclusive) to max – min (exclusive). Adding min to that gives the range min (inclusive) to
      // max (exclusive). Finally, to include max, we add 1 to the range.
      .ints(6, lo, hi + 1)
      .mapToObj(_.asInstanceOf[Integer].toChar.toString)
      .collect(Collectors.joining())
  }

  /*
   * Given a string s and a non-empty string p, find all the start indices of p's anagrams in s.
   * Strings consists of lowercase English letters only and the length of both strings s and p will not be larger
   * than 20,100.
   * The order of output does not matter.
   *
   * Example 1:
   * Input: s: "cbaebabacd" p: "abc"
   * Output: [0, 6]
   * Explanation: The substring with start index = 0 is "cba", which is an anagram of "abc". The substring with start
   * index = 6 is "bac", which is an anagram of "abc".
   * Example 2:
   * Input: s: "abab" p: "ab"
   * Output: [0, 1, 2]
   * Explanation: The substring with start index = 0 is "ab", which is an anagram of "ab". The substring with start
   * index = 1 is "ba", which is an anagram of "ab". The substring with start index = 2 is "ab", which is an anagram
   * of "ab".
   *
   * ANSWER: Let s.length = n and p.length = m. A naive way of solving this problem is to compare every m length
   * substring of s (there are n - m + 1) with p after sorting both. Time complexity is O(nm log m). Can we do better?
   *
   * Observe that each consecutive m length substring of s only differs by a single character. Thus, if we are able to
   * take advantage of the work done in the previous iteration, we can save time. Instead of sorting and comparing, we
   * start by creating the frequency maps for p and the first m length substring of s. If the maps are equal, we add
   * the first substring to the results. Then for each subsequent substring, we only update the entry for the character
   * that moved out of the sliding window, and add the entry for the incoming character. If the resulting frequency map
   * is equal to the one initially created for p, then we have found an anagram of p.
   * If all characters in p are unique, outside the loop, we do O(m) work. The loop runs n - m times, and we go over a
   * map of size m each time; overall time complexity is O(m) + O(nm - m^2). If m is small compared to n, the second
   * term dominates and time complexity is O(n). If m is comparable to m, the first time dominates and time complexity
   * is still O(n).
   */
  def findAnagrams(s: String, p: String): List[Int] = {
    def freq(str: String): Map[Char, Int] = str
      .map((_, 1))
      .groupBy(_._1)
      .mapValues(_.size)

    val m = p.length
    val a = s.take(m)
    val x = freq(a)
    val y = freq(p)
    val z = y.toSet
    val result = ListBuffer.empty[Int]
    if (x == y) result.append(0)

    (1 to s.length - m)
      .foldLeft(x) { case (f1, i) =>
        val prev = s(i - 1)
        val f2 = if (f1(prev) == 1) f1 - prev else f1 + (prev -> (f1(prev) - 1))
        val cur = s(i + m - 1)
        val f3 = f2 + (cur -> (f2.getOrElse(cur, 0) + 1))

        if (z.diff(f3.toSet).isEmpty)
          result.append(i)
        f3
      }
    result
      .toList
  }

  /*
   * Given a list of unique words, find all pairs of distinct indices (i, j) in the given list, so that the
   * concatenation of the two words, i.e. words[i] + words[j] is a palindrome.
   *
   * ANSWER: words[i] + words[j] is a palindrome in one of the two cases:
   * 1. They are of equal length, and one is the reverse of the other. Example, "abc" and "cba".
   * 2. The reverse of the shorter string is a suffix of the longer one, and the remaining prefix is a palindrome.
   *    Example, "abc" and "ddcba".
   *
   * Since the empty string is considered a palindrome, we can combine the above conditions into the second one.
   *
   * Thus, we need an algorithm for condition #2. If we build a Trie using the reverse strings, we can efficiently
   * check for prefix matches. Once we have a match, to check whether the rest of the string is a palindrome or not,
   * we store that information while building the Trie. Each node stores a list indices corresponding to the words
   * in the input list if the rest of the string is a palindrome.
   *
   * The Trie for "abc" and "ddcba" would be as follows:
   *
   *     +--+a
   *     +   +
   *     c   b
   *     +   +
   *     +   c [1]
   *     b   +
   *     +   d
   * [0] a   +
   *         d [1]
   *
   * The algorithm is then simply finding the node where a word ends, and adding the pairs (index, other index) to
   * the answer. Note that the order of elements in the pair is important.
   *
   * Time complexity: Let n be the number of words, and m the length of the longest word. Since we scan the rest of the
   * string for each index position, we spend O(m^2) time inserting the word in the Trie. Finding it is O(m); thus,
   * overall time complexity is O(nm + nm^2).
   */
  def palindromePairs(words: Seq[String]): Seq[(Int, Int)] = {
    val trie = words
      .zipWithIndex
      .foldLeft(new PalindromePairTrie()) { case (tr, (w, i)) => tr += (w, i) }

    words
      .zipWithIndex
      .flatMap { case (w, i) => trie.get(w) match {
        case Some(node) => node.palin.filterNot(_ == i).map((i, _))
        case _ => Seq.empty
      }
      }
  }

  /*
   * The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this:
   * P   A   H   N
   * A P L S I I G
   * Y   I   R
   *
   * And then read line by line: "PAHNAPLSIIGYIR"

   * Write the code that will take a string and make this conversion given a number of rows.
   *
   * ANSWER: Let m be the number of rows. Observe that for every m-th character, the direction is reversed. Also
   * observe that when the index of the current character is an even multiple of m, the direction is changed downward,
   * and when the index is an odd multiple of m, the direction is changed upward. With these observations, all we need
   * to do is keep m buffers, and using the above logic, figure out which one to write to next.
   *
   * Time complexity: O(n), where n is the length of the input string.
   */
  def zigZag(s: String, numRows: Int): String = {
    val m = numRows - 1
    if (m == 0) s
    else {
      val rows = IndexedSeq.fill(numRows)(new StringBuilder)

      Iterator.iterate((1, 0, 0)) { case (rowOffset, row, i) =>
        val j = if (i % m == 0) {
          if ((i / m) % 2 == 0) 1 else -1
        } else rowOffset

        (j, row + j, i + 1)
      }
        .takeWhile(x => s.isDefinedAt(x._3))
        .foreach(x => rows(x._2).append(s(x._3)))

      rows
        .reduce((s1, s2) => s1.append(s2.toString()))
        .toString()
    }
  }

  /*
   * Lexicographically least circular substring is the problem of finding the rotation of a string possessing the
   * lowest lexicographical order of all such rotations. For example, the lexicographically minimal rotation of
   * "bbaaccaadd" would be "aaccaaddbb".
   *
   * A O(n) time algorithm was proposed by Jean Pierre Duval (1983).
   *
   * Given two indices i and j, Duval's algorithm compares string segments of length j - i starting at i and j (called
   * a "duel"). If (index + j - i) is greater than the length of the string, the segment is formed by wrapping around.
   *
   * For example, consider s = "baabbaba", i = 5 and j = 7. Since j - i = 2, the first segment starting at i = 5 is
   * "ab". The second segment starting at j = 7 is constructed by wrapping around, and is also "ab".
   * If the strings are lexicographically equal, like in the above example, we choose the one starting at i as the
   * winner, which is i = 5.
   *
   * The above process repeated until we have a single winner. If the input string is of odd length, the last character
   * wins without a comparison in the first iteration.
   *
   * Time complexity:
   * The first iteration compares n strings each of length 1 (n/2 comparisons), the second iteration may compare n/2
   * strings of length 2 (n/2 comparisons), and so on, until the i-th iteration compares 2 strings of length n/2
   * (n/2 comparisons). Since the number of winners is halved each time, the height of the recursion tree is log(n),
   * thus giving us a O(n log(n)) algorithm. For small n, this is approximately O(n).
   *
   * Space complexity is O(n) too, since in the first iteration, we have to store n/2 winners, second iteration n/4
   * winners, and so on. (Wikipedia claims this algorithm uses constant space, I don't understand how).
   *
   * See https://blog.asarkar.org/coding-interview-curated/#strings for additional reference materials.
   */
  def lexicographicallyMinRotation(s: String): String = {
    @tailrec
    def duel(winners: Seq[Int]): String = {
      if (winners.size == 1) s"${s.slice(winners.head, s.length)}${s.take(winners.head)}"
      else {
        val newWinners: Seq[Int] = winners
          .sliding(2, 2)
          .map {
            case Seq(x, y) =>
              val range = y - x
              Seq(x, y)
                .map { i =>
                  val segment = if (s.isDefinedAt(i + range - 1)) s.slice(i, i + range)
                  else s"${s.slice(i, s.length)}${s.take(s.length - i)}"
                  (i, segment)
                }
                .reduce((a, b) => if (a._2 <= b._2) a else b)
                ._1
            case xs => xs.head
          }
          .toSeq
        duel(newWinners)
      }
    }

    duel(s.indices)
  }

  case class MaxXorTrie(children: mutable.Map[Char, MaxXorTrie] = mutable.Map.empty) {
    def insert(str: String): Unit = {
      @tailrec
      def insert(i: Int, node: MaxXorTrie): Unit = {
        if (str.isDefinedAt(i)) {
          val ch = str(i)
          if (!node.children.contains(ch)) node.children(ch) = MaxXorTrie()
          insert(i + 1, node.children(ch))
        }
      }

      insert(0, this)
    }

    def find(ch: Char): Option[MaxXorTrie] = children.get(ch)

    override def toString: String = children.toString()
  }

  private implicit class IntString(x: Int) {
    def toBinStr(width: Int): String = {
      val bin = Integer.toBinaryString(x)
      val numZeros = width - bin.length
      s"""${"0" * numZeros}$bin"""
    }
  }

  /*
   * Given a non-empty array of numbers, a0, a1, a2, ..., where 0 <= ai < 2^31.
   * Find the maximum result of ai XOR aj, where 0 <= i, j < n.
   * Could you do this in O(n) runtime?
   * Example:
   * Input: [3, 10, 5, 25, 2, 8]
   * Output: 28
   * Explanation: The maximum result is 5 ^ 25 = 28
   *
   * ANSWER: We make three passes. In the first pass, we find the maximum number in the array, and calculate its length
   * in binary; let's call it 'len'. For example, 25 = 11001, len = 5.
   * In the second pass, we convert each number to a binary string of length 'len', and insert into a Trie.
   * In the third and final pass, we go over each binary string (say 's') again, and for each bit, look for its
   * opposite bit in the Trie. This is because we want as many '1's in the max XOR as possible, and by definition,
   * the XOR of two bits is 1 only when they are different. If we find the opposite bit, then the XOR of 's'
   * with the number(s) being traversed has a '1' at this position, so we calculate the decimal equivalent, and add to
   * the running XOR. If we don't find the opposite bit, then the XOR of 's' has a '0' as this position, and we don't
   * change the running XOR.
   */
  def maxXor(xs: IndexedSeq[Int]): Int = {
    val root = MaxXorTrie()
    val maxLen = Integer.toBinaryString(xs.max).length
    val ys = xs
      .map { i =>
        val j = i.toBinStr(maxLen)
        root.insert(j)
        j
      }

    ys
      .foldLeft(Int.MinValue) { case (max, bin) =>
        val xor = bin
          .zipWithIndex
          .foldLeft((Option(root), 0)) { case ((node, j), (ch, i)) =>
            val other = if (ch == '0') '1' else '0'
            node.flatMap(_.find(other)) match {
              case None => (node.flatMap(_.find(ch)), j)
              case x => (x, (1 << (maxLen - 1 - i)) + j)
            }
          }
          ._2
        math.max(max, xor)
      }
  }

  /*
   * Given a string and a pattern, find the starting indices of all occurrences of the pattern in the string.
   *
   * For example, given the string "abracadabra" and the pattern "abr", you should return [0, 7].
   *
   * ANSWER: Let n be the length of the string s, and m the length of the pattern p. We can take every substring of s
   * of length m, and compare with p. If they match, we add the starting index of the substring to the result.
   * This works, but there are n - m + 1 such substring, and comparing each pair would take m comparisons, thus giving
   * us a O(mn - m^2 + m) time. Can we do better?
   *
   * We can! The key observation is that each substring is generated by a sliding window of size m that moves one step
   * to the right each time. m - 1 characters in that window don't change; the left one drops off, and a new one
   * comes in at the right. Thus, we are wasting time comparing the complete substring over and over again.
   *
   * Instead, we calculate the hash of the first window using a hash function. For each subsequent window, we update
   * the hash using the rolling hash function. Calculating the first hash takes O(m) time, and all subsequent ones
   * takes constant time to calculate.
   * The loop runs n - m times, giving us an overall time complexity of O(m) + O(n - m). For n >> m, this is
   * practically O(n).
   *
   * Due to hash collisions, we should still compare the substrings if the hashes match. With a bad hashing function,
   * that still gives us the same worst came running time as the naive algorithm, but in reality, we can choose a good
   * hash function to reduce the chances of collision.
   *
   * This is known as the Rabin-Karp algorithm.
   */
  def findAllOccurrences(s: String, p: String): Seq[Int] = {
    val n = s.length
    val m = p.length
    val prime = 101

    def hash(str: String): Int = {
      (0 until m)
        .map(i => str(i) * math.pow(prime, i).toInt)
        .sum
    }

    def rehash(prevHash: Int, i: Int): Int = {
      (prevHash - s(i - 1)) / prime + (s(i + m - 1) * math.pow(prime, m - 1).toInt)
    }

    val initialHash = hash(s)
    val pHash = hash(p)

    (0 to n - m)
      .foldLeft((initialHash, mutable.ListBuffer.empty[Int])) { case ((hash, buf), i) =>
        val rollingHash = if (i == 0) hash else rehash(hash, i)
        if (rollingHash == pHash) buf.append(i)
        (rollingHash, buf)
      }
      ._2
  }

  /*
   * Ghost is a two-person game game where players alternate appending letters to a word. The first person who spells
   * out a dictionary word, or creates a prefix for which there is no possible continuation, loses. Here is a sample
   * game:
   *
   * +----------+---------+
   * |   Turn   |  Letter |
   * +----------+---------+
   * | Player 1 | g       |
   * | Player 2 | h       |
   * | Player 1 | o       |
   * | Player 2 | s       |
   * | Player 1 | t       |
   * +----------+---------+
   *
   * Given a dictionary of words, determine the letters the first player should start with, such they with optimal play
   * they cannot lose.
   *
   * For example, if the dictionary is ["cat", "calf", "dog", "bear"], the only winning start letter would be 'b'.
   *
   * ANSWER: Observe that the starting player wins only if the game ends with an even-length word. Thus, any letter
   * that may lead to an odd-length word is not a guaranteed winning combination.
   * Time complexity: O(n), where n is the number of words in the dictionary. Space complexity: O(nk), where k is the
   * average length of a word.
   */
  def winningLettersForGhost(words: Seq[String]): Seq[Char] = {
    words
      .groupBy(_.head)
      .filterNot(_._2.exists(_.length % 2 != 0))
      .keys
      .toSeq
  }

  /*
   * Given a string S and a string T, find the minimum window in S which will contain all the characters in T in
   * complexity O(n).
   *
   * Example:
   * Input: S = "ADOBECODEBANC", T = "ABC"
   * Output: "BANC"
   *
   * Note:
   * If there is no such window in S that covers all characters in T, return the empty string "".
   * If there is such window, you are guaranteed that there will always be only one unique minimum window in S.
   *
   * ANSWER: We start a sliding window, and keep track of whether the window contains all chars from "t".
   * Note than "t" may have duplicates, and all chars, including dupes, must be present in the min window. For example,
   * if s = "aabc", and t = s, the min window string is "aabc", not "abc".
   *
   * To determine if the current window is a potential solution, we keep a char freq map of "t" as reference. We
   * maintain another freq map for the current window for the chars present in "t". At each iteration, we try to
   * extend the window to the right by one char only if the incoming char is present in "t". If successful,
   * we try to shrink the window from the left as long as it doesn't invalidate the current solution.
   *
   * For example, let s = "aabc", t = "abc". We start with the window = "a", which is not a solution. Since the next
   * char 'a' is present in "t", we take it in, and the next char 'b', and the next char 'c'; the window is now "aabc".
   * This is a solution, but not the min one. We then try to shrink the window without invalidating the solution,
   * and we can drop 'a' from the left.
   *
   * To avoid comparing the frequency maps, we keep a variable count that is incremented once for each char from
   * "t" when we have seen the char as many times as present in "t". For example, t = "aabc", count would be
   * incremented when we have seen two counts of 'a', one count of 'b', and one count of 'c'. When count is equal to
   * the size of the reference frequency map, we have a candidate solution. We then check if the current window is
   * smaller than the current min window, and if yes, update the boundaries.
   *
   * Time complexity: O(n). The for loop runs n times. Although it may look like the nested while loop adds to the time
   * complexity, it only runs n times, because start is incremented inside it, and start can only be less than n.
   */
  def minWindow(s: String, t: String): String = {
    val m = t.length

    if (s.length < m) ""
    else {
      val ref = t
        .groupBy(identity)
        .mapValues(_.length)

      val freq = mutable.Map.empty[Char, Int]
      var count = 0
      var found = false
      var start = 0
      var (minStart, minEnd, min) = (-1, -1, Int.MaxValue)

      for {
        end <- s.indices
        incoming = s(end)
        if ref.contains(incoming)
      } {
        freq(incoming) = freq.getOrElse(incoming, 0) + 1
        if (freq.get(incoming) == ref.get(incoming)) count += 1


        while (end - start + 1 > m && (!freq.contains(s(start)) || freq(s(start)) > ref(s(start)))) {
          if (freq.contains(s(start))) freq(s(start)) = freq(s(start)) - 1
          start += 1
        }

        if (((freq.size == ref.size) && (count == ref.size)) && (end - start + 1) < min) {
          found = true
          minStart = start
          minEnd = end
          min = minEnd - minStart + 1
        }
      }
      if (found) s.slice(minStart, minEnd + 1) else ""
    }
  }

  /*
   * Given two strings A and B, return whether or not A can be shifted some number of times to get B.
   *
   * For example, if A is abcde and B is cdeab, return true. If A is abc and B is acb, return false.
   *
   * ANSWER: A + A contains B. Time complexity: O(n).
   */

  /*
   * Given a non-empty string s, you may delete at most one character. Judge whether you can make it a palindrome.
   *
   * Example 1:
   * Input: "aba"
   * Output: True
   *
   * Input: "abca"
   * Output: True
   * Explanation: You could delete the character 'c'.
   *
   * ANSWER: We examine the characters both from ends. When we find a non-matching pair, either character could be
   * deleted to form a palindrome. We check both possibilities.
   *
   * Time complexity: O(n), where n is the length of the string.
   */
  def validPalindrome(s: String): Boolean = {
    def indexOfNoMatch(start: Int, end: Int): Int = {
      val mid = (end - start) / 2
      Iterator.from(0)
        .takeWhile(_ <= mid)
        .find(i => (start + i < end - i) && s(start + i) != s(end - i))
        .getOrElse(-1)
    }

    val x = indexOfNoMatch(0, s.length - 1)
    x == -1 || (indexOfNoMatch(x + 1, s.length - 1 - x) == -1) || (indexOfNoMatch(x, s.length - 2 - x) == -1)
  }

  /*
   * Given a string, return whether it represents a number. Here are the different kinds of numbers:
   * "10", a positive integer
   * "-10", a negative integer
   * "10.1", a positive real number
   * "-10.1", a negative real number
   * "1e5", a number in scientific notation
   *
   * And here are examples of non-numbers:
   * "a"
   * "x 1"
   * "a -2"
   * "-"
   *
   * ANSWER: The fastest solution would be using a NFA (regex), but the one below is far simpler. The main challenge
   * is to come up with the requirements for what is considered valid. The following implementation is based on the
   * test cases for LeetCode #65.
   * Time complexity is O(n), although we may traverse the string few times.
   */
  def isValidNumber(str: String): Boolean = {
    // xey means x * 10^y
    def isValidScientific(s: String): Boolean = {
      val tokens = s.split("e", 2)
      // doesn't contain 'e'
      if (tokens.length == 1) isValidFloat(s)
      // can't start or end with an 'e'
      else if (tokens(0).isEmpty || tokens(1).isEmpty) false
      // segment after 'e' must be an integer
      else isValidFloat(tokens(0)) && isValidInt(tokens(1))
    }

    def isValidUnsignedFloat(s: String): Boolean = {
      // ok to start or end with a '.'
      s.split("\\.", 2)
        .filter(_.nonEmpty) match {
        case xs if xs.nonEmpty => xs.forall(isValidUnsignedInt)
        // can't be only '.'
        case _ => false
      }
    }

    def isValidFloat(s: String): Boolean = {
      // can't use dropWhile because s may start with more than one sign
      if (s.startsWith("+") || s.startsWith("-")) isValidUnsignedFloat(s.drop(1))
      else isValidUnsignedFloat(s)
    }

    def isValidUnsignedInt(s: String) = s.matches("\\d+")

    def isValidInt(s: String) = s.matches("[+-]?\\d+")

    val trimmed = str.trim
    trimmed.nonEmpty && isValidScientific(trimmed)
  }
}

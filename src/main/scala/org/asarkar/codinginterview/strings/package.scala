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
}

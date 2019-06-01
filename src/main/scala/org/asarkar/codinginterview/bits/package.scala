package org.asarkar.codinginterview

import scala.annotation.tailrec

package object bits {
  /*
   * Given an array of integers where every integer occurs three times except for one integer, which only occurs once,
   * find and return the non-duplicated integer.
   *
   * For example, given [6, 1, 3, 3, 3, 6, 6], return 1. Given [13, 19, 13, 13], return 19.
   *
   * Do this in O(N) time and O(1) space.
   *
   * ANSWER: We need an operator that cancels out n occurrences of a integer but keeps one occurrence. If we convert
   * each number to its binary representation, and for each bit position, count the number of times that this bit is
   * set, the value will be a multiple of n for all numbers that occur n times, plus 0 or 1 for the corresponding
   * bit of the lone integer.
   * If we then take modulo n of each of these counts, the remainder is the value of the corresponding bit position for
   * the lone integer. All that's left is to convert the binary result to decimal formal.
   *
   * For example, given xs = [3, -4, 3, 3] and n = 3:
   * Binary of 3 = 011
   * Binary of -4 = 11111111111111111111111111111100
   *   Binary of 4 is 100, to get negative in 2s complement, invert bits and add 1
   *   (29 more 1s)011 + 1 = (29 more 1s)100
   *
   *   See https://www.cs.cornell.edu/~tomf/notes/cps104/twoscomp.html#whyworks
   *
   *   For 2s complement subtraction, each output digit is a XOR of three things: The two input digits and the
   *   carry-in/borrow-in.
   *
   * Bit 0 count % 3: 3 % 3 = 0
   * Bit 1 count % 3: 3 % 3 = 0
   * Bit 2 through 32 count % 3: 1 % 3 = 1
   * The result is -4
   */
  def loneInt(xs: Seq[Int], n: Int): Int = {
    val loner = xs
      .foldLeft(Array.ofDim[Int](Integer.SIZE)) { (freq, i) =>
        freq.indices
          .filter(j => (i >> j & 1) == 1)
          .foreach(freq(_) += 1)
        freq
      }
      .map(_ % n)
      // mkString puts the LSD first which is not what we want
      .foldRight(new StringBuilder())((i, sb) => sb.append(i))
      .toString()

    // The requirement that Integer.parseInt() be given a signed number means that it's unsuitable as-is for 32-bit
    // binary strings with a leftmost 1-bit - a NumberFormatException is thrown. We parse it into a long and then take
    // the int value
    java.lang.Long.parseLong(loner, 2).intValue()
  }

  /*
   * Given three 32-bit integers x, y, and b, return x if b is 1 and y if b is 0, using only mathematical or bit
   * operations. You can assume b can only be 1 or 0.
   */
  def thisOrThat(x: Int, y: Int, b: Int): Int = {
    val c = -b // if b = 1, -1 is all 1's
    (x & c) | (y & ~c)
  }

  /*
   * A character in UTF8 can be from 1 to 4 bytes long, subjected to the following rules:
   * For 1-byte character, the first bit is a 0, followed by its unicode code.
   * For n-bytes character, the first n-bits are all one's, the n+1 bit is 0, followed by n-1 bytes with most
   * significant 2 bits being 10.
   *
   * Given an array of integers representing the data, return whether it is a valid utf-8 encoding.
   * Note: The input is an array of integers. Only the least significant 8 bits of each integer is used to store the
   * data. This means each integer represents only 1 byte of data.
   *
   * Example 1:
   * data = [197, 130, 1], which represents the octet sequence: 11000101 10000010 00000001.
   * Return true.
   * It is a valid utf-8 encoding for a 2-bytes character followed by a 1-byte character.
   *
   * Example 2:
   * data = [235, 140, 4], which represented the octet sequence: 11101011 10001100 00000100.
   * Return false.
   * The first 3 bits are all one's and the 4th bit is 0 means it is a 3-bytes character.
   * The next byte is a continuation byte which starts with 10 and that's correct.
   * But the second continuation byte does not start with 10, so it is invalid.
   */
  def isValidUtf8(xs: Seq[Int]): Boolean = {
    @tailrec
    def isValid(start: Int): Boolean = {
      val n = xs.length

      if (start >= n) true
      else if ((xs(start) & (1 << 7)) == 0) isValid(start + 1) // 0xxxxxxx, 1-byte
      else {
        val i = if ((xs(start) >> 5) == 6) 2 // 110xxxxx, 2-bytes
        else if ((xs(start) >> 4) == 14) 3 // 1110xxxx, 3-bytes
        else if ((xs(start) >> 3) == 30) 4 // 11110xxx, 4-bytes
        else -1 // invalid

        if (i < 2 || (start + i - 1) >= n) false // invalid or not enough continuation char
        else (1 until i).forall(j => (xs(start + j) >> 6) == 2) && // 10xxxxxx, continuation char
          isValid(start + i)
      }
    }

    isValid(0)
  }

  /*
   * Given an unsigned 8-bit integer, swap its even and odd bits. The 1st and 2nd bit should be swapped, the 3rd and
   * 4th bit should be swapped, and so on.
   * For example, 10101010 should be 01010101. 11100010 should be 11010001.

   * Bonus: Can you do this in one line?
   *
   * ANSWER: We apply a bitmask over all the even bits, and another one over all the odd bits. Then we shift the even
   * bitmask right by one and the odd bitmask left by one.
   * (x & 0b10101010) >> 1 | (x & 0b01010101) << 1
   *
   * Time complexity: O(1), since the integer is a constant number of bits (8).
   */
}

package org.asarkar.codinginterview

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
        val bin = i.toBinaryString
        bin
          .zipWithIndex
          .filter(_._1.asDigit == 1)
          .map(x => bin.length - 1 - x._2)
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
}

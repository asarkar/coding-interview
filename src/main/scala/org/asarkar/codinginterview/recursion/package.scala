package org.asarkar.codinginterview

package object recursion {
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
    val dp = Array.fill[Int](msg.length + 1)(-1)
    val (lowest, highest) = (1, 26)

    def loop(i: Int): Int = {
      if (i == 0) 1
      else {
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

  //  val hamming: Stream[BigInt] = {
  //    // #::[B >: A](hd: B): Stream[B]
  //    // Construct a stream consisting of a given first element followed by elements from a lazily evaluated Stream.
  //    def merge(inx: Stream[BigInt], iny: Stream[BigInt]): Stream[BigInt] = {
  //      if (inx.head < iny.head) inx.head #:: merge(inx.tail, iny)
  //      else if (iny.head < inx.head) iny.head #:: merge(inx, iny.tail)
  //      else merge(inx, iny.tail)
  //    }
  //
  //    1 #:: merge(hamming.map(_ * 2), merge(hamming.map(_ * 3), hamming.map(_ * 5)))
  //  }
}

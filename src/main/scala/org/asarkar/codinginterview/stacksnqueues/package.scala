package org.asarkar.codinginterview

import scala.collection.mutable.ListBuffer

package object stacksnqueues {
  /*
   * Given a string of round, curly, and square open and closing brackets, return whether the brackets are balanced
   * (well-formed).
   * For example, given the string "([])[]({})", you should return true.
   * Given the string "([)]" or "((()", you should return false.
   *
   * ANSWER: Put the opening brackets on a stack, pop whenever we see a closing one, and compare if it pairs with the
   * item at the top of the stack.
   */

  /*
   * Implement a stack that has the following methods:
   *
   * - push(val), which pushes an element onto the stack
   * - pop(), which pops off and returns the topmost element of the stack. If there are no elements in the stack, then
   *   it should throw an error or return null.
   * - max(), which returns the maximum value in the stack currently. If there are no elements in the stack, then it
   *   should throw an error or return null.
   *
   * Each method should run in constant time.
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/stacks/StackWithMax.scala
   */

  /*
   * Given a list of possibly overlapping intervals, return a new list of intervals where all overlapping intervals
   * have been merged.
   *
   * The input list is not necessarily ordered in any way.
   *
   * For example, given [(1, 3), (5, 8), (4, 10), (20, 25)], you should return [(1, 3), (4, 10), (20, 25)].
   *
   * ANSWER: We sort the intervals by start times (and end times, if start times are equal). Then we iterate the sorted
   * sequence and for each one, check if it overlaps with the previous one. If yes, we merge them; if not, we add the
   * current interval to the result.
   */
  def mergeOverlapping(xs: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    xs.sorted match {
      case head +: tail => tail.foldLeft(ListBuffer(head)) { (queue, next) =>
        if (next._1 <= queue.last._2 && next._2 >= queue.last._2) {
          val top = queue.remove(queue.size - 1)
          queue += ((top._1, next._2))
        } else if (next._1 > queue.head._2) queue += next
        else queue
      }
      case _ => xs
    }
  }

  /*
   * Given a string of parentheses, write a function to compute the minimum number of parentheses to be removed to make
   * the string valid (i.e. each open parenthesis is eventually closed).
   *
   * For example, given the string "()())()", you should return 1. Given the string ")(", you should return 2, since we
   * must remove all of them.
   */

  def numParenthesesToRemove(expr: String): Int = {
    val x = expr.foldLeft((ListBuffer.empty[Char], 0)) { case ((stack, count), ch) =>
      if (ch == '(') (ch +=: stack, count)
      else if (stack.nonEmpty) (stack.drop(1), count)
      else (stack, count + 1)
    }
    x._2 + x._1.size
  }

  /*
   * Generate all prime numbers less than a non-negative number, n.
   * Example:
   * Input: 10
   * Output: 2, 3, 5, 7
   *
   * Explanation: There are 4 prime numbers less than 10, they are 2, 3, 5, 7.
   *
   * ANSWER: We use Sieve of Eratosthenes, but instead of crossing out all the multiples of a prime, we put it's
   * square on a priority queue, along with the prime, such that we can generate the latter multiples when we need to.
   * The next smallest composite is always on the top, so we can check if a number if prime or not in O(1) time.
   *
   * Primes are returned in sorted order.
   *
   * https://www.khanacademy.org/computing/computer-science/cryptography/comp-number-theory/v/sieve-of-eratosthenes-prime-adventure-part-4
   *
   * Time complexity: The outer loop runs n - 2 times, and the if-else does a constant number of O(log n) operations.
   * Overall time complexity is O(n log n).
   */
  def allPrimes(n: Int): collection.Set[Int] = {
    val x = math.sqrt(n)

    (2 until n)
      .foldLeft((collection.mutable.LinkedHashSet.empty[Int], new java.util.PriorityQueue[(Int, Int)](Ordering.by[(Int, Int), Int](_._1)))) {
        case ((primes, composites), i) =>
          if (composites.isEmpty || composites.peek()._1 != i) {
            /* Suppose xy = n = √n√n. If x ≥ √n, then y ≤ √n and vice-versa. Thus, if xy = n, then one of x or y must
             * be less than or equal to √n. This means that if n can be factored, one of the factors must be less
             * than or equal to √n.
             * If we have crossed out the multiples of all the numbers less than or equal to √n, all multiples of
             * numbers greater than √n will already be crossed out. This is because any number which is less than or
             * equal to n and is a multiple of a number greater than √n, will have a factor that is less than or equal
             * to √n and therefore will already be crossed out (in the original method, when we find a prime, we cross
             * out all multiples of it <= n).
             * Thus, putting only composites <= √n on the pq is sufficient.
             */
            val j = i * i
            if (i <= x) composites.offer((j, i))
            (primes += i, composites)
          } else {
            // there may be more than one entry with the same value as i; e.g. 12 is a multiple of 2 as well as 3,
            // so both (12, 2) from 10 + 2 and (12, 3) from 9 + 3 can be in the PQ.
            while (!composites.isEmpty && composites.peek()._1 == i) {
              val (j, k) = composites.remove()
              if (j + k <= n) composites.offer((j + k, k))
            }

            (primes, composites)
          }
      }
      ._1
  }

  /*
   * Given a string of words delimited by spaces, reverse the words in string. For example, given "hello world here",
   * return "here world hello"
   *
   * Follow-up: given a mutable string representation, can you perform this operation in-place?
   *
   * ANSWER: It's trivial to solve this using String.split(\\s+); Without using split, we simply find word boundaries
   * at each iteration and put the word in between those boundaries on a stack.
   * Time complexity: O(n).
   */
  def reverseWords(s: String): String = {
    Iterator.iterate((0, ListBuffer.empty[String])) { case (start, stack) =>
      val x = Iterator.from(start)
        .dropWhile(i => s.lift(i).exists(_.isSpaceChar))
        .take(1)
        .next()

      val y = Iterator.from(x)
        .dropWhile(i => s.lift(i).exists(!_.isSpaceChar))
        .take(1)
        .next()

      if (x < y && y <= s.length) s.substring(x, y) +=: stack

      (y, stack)
    }
      .dropWhile(x => s.isDefinedAt(x._1))
      .take(1)
      .next()
      ._2
      .mkString(" ")
  }

  /*
   * Given a string and a set of delimiters, reverse the words in the string while maintaining the relative order of
   * the delimiters. For example, given "hello/world:here", return "here/world:hello"
   *
   * Follow-up: Does your solution work for the following cases: "hello/world:here/", "hello//world:here"
   *
   * ANSWER: Same as above, except to maintain the relative ordering of the delimiters, we put them on a queue.
   */
  def reverseWords(s: String, del: Set[Char]): String = {
    val (a, b) = Iterator.iterate((0, ListBuffer.empty[String], ListBuffer.empty[String])) { case (start, stack, queue) =>
      val x = Iterator.from(start)
        .dropWhile(i => s.lift(i).exists(del.contains))
        .take(1)
        .next()

      if (x > start) queue += s.substring(start, x)

      val y = Iterator.from(x)
        .dropWhile(i => s.lift(i).exists(!del.contains(_)))
        .take(1)
        .next()

      if (x < y && y <= s.length) s.substring(x, y) +=: stack

      (y, stack, queue)
    }
      .dropWhile(x => s.isDefinedAt(x._1))
      .take(1)
      .map(x => (x._2, x._3))
      .next()

    a.zipAll(b, "", "")
      .map(x => x._1 + x._2)
      .mkString
  }
}

package org.asarkar.codinginterview

import java.util.concurrent.ThreadLocalRandom

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

package object numerics {
  /*
     * Compute the first n Hamming number (https://en.wikipedia.org/wiki/Regular_number).
     *
     * ANSWER: Recall axiom 2 in Dijkstra's original paper (https://blog.asarkar.org/assets/docs/algorithms-curated/Hamming's%20Problem%20-%20Dijkstra.pdf):
     * Axiom 2. If x is in the sequence, so is 2 * x, 3 * x, and 5 * x.
     * After some whiteboarding, it became clear that the axiom 2 is not an invariant at each iteration of the algorithm,
     * but actually the goal of the algorithm itself. At each iteration, we try to restore the condition in axiom 2.
     * If last is the last value in the result sequence S, axiom 2 can simply be rephrased as:
     * For some x in S, the next value in S is the minimum of 2x,  3x, and 5x, that is greater than last.
     * Let's call this axiom 2'.
     *
     * Thus, if we can find x, we can compute the minimum of 2x, 3x, and 5x in constant time, and add it to S.
     * But how do we find x? One approach is, we don't; instead, whenever we add a new element e to S, we compute 2e, 3e,
     * and 5e, and add them to a minimum priority queue. Since this operations guarantees e is in S, simply extracting
     * the top element of the PQ satisfies axiom 2'.
     *
     * This approach works, but the problem is that we generate a bunch of numbers we may not end up using. For example:
     * +---------+--------------------------------+-------------+
     * | #       | PQ                             | S           |
     * +---------+--------------------------------+-------------+
     * | initial | {2,3,5}                        | {1}         |
     * +---------+--------------------------------+-------------+
     * | 1       | {3,4,5,6 10}                   | {1,2}       |
     * +---------+--------------------------------+-------------+
     * | 2       | {4,5,6,6,9,10,15}              | {1,2,3}     |
     * +---------+--------------------------------+-------------+
     * | 3       | {5,6,6,8,9,10,12,15,20}        | {1,2,3,4}   |
     * +---------+--------------------------------+-------------+
     * | 4       | {6,6,8,9,10,10,12,15,15,20,25} | {1,2,3,4,5} |
     * +---------+--------------------------------+-------------+
     *
     * If we want the 5th element in S (5), the PQ at that moment holds 6,6,8,9,10,10,12,15,15,20,25. Can we not waste
     * this space?
     *
     * Turns out, we can do better. Instead of storing all these numbers, we simply maintain three counters for each of
     * the multiples, namely, 2i, 3j, and 5k. These are candidates for the next number in S. When we pick one of them,
     * we increment only the corresponding counter, and not the other two. By doing so, we are not eagerly generating
     * all the multiples, thus solving the space problem with the first approach.
     *
     * Let's see a dry run for n = 8, i.e. the number 9. We start with 1, as stated by axiom 1 in Dijkstra's paper.
     *
     * +---------+---+---+---+----+----+----+-------------------+
     * | #       | i | j | k | 2i | 3j | 5k | S                 |
     * +---------+---+---+---+----+----+----+-------------------+
     * | initial | 1 | 1 | 1 | 2  | 3  | 5  | {1}               |
     * +---------+---+---+---+----+----+----+-------------------+
     * | 1       | 1 | 1 | 1 | 2  | 3  | 5  | {1,2}             |
     * +---------+---+---+---+----+----+----+-------------------+
     * | 2       | 2 | 1 | 1 | 4  | 3  | 5  | {1,2,3}           |
     * +---------+---+---+---+----+----+----+-------------------+
     * | 3       | 2 | 2 | 1 | 4  | 6  | 5  | {1,2,3,4}         |
     * +---------+---+---+---+----+----+----+-------------------+
     * | 4       | 3 | 2 | 1 | 6  | 6  | 5  | {1,2,3,4,5}       |
     * +---------+---+---+---+----+----+----+-------------------+
     * | 5       | 3 | 2 | 2 | 6  | 6  | 10 | {1,2,3,4,5,6}     |
     * +---------+---+---+---+----+----+----+-------------------+
     * | 6       | 4 | 2 | 2 | 8  | 6  | 10 | {1,2,3,4,5,6}     |
     * +---------+---+---+---+----+----+----+-------------------+
     * | 7       | 4 | 3 | 2 | 8  | 9  | 10 | {1,2,3,4,5,6,8}   |
     * +---------+---+---+---+----+----+----+-------------------+
     * | 8       | 5 | 3 | 2 | 10 | 9  | 10 | {1,2,3,4,5,6,8,9} |
     * +---------+---+---+---+----+----+----+-------------------+
     *
     * Notice that S didn't grow at iteration 6, because the minimum candidate 6 had already been added previously.
     * To avoid this problem of having to remember all of the previous elements, we amend our algorithm to increment all
     * the counters whenever the corresponding multiples are equal to the minimum candidate.
     * That brings us to the following implementation.
     *
     * Time Complexity: O(n)
     */
  def hamming(n: Int): Seq[BigInt] = {
    @tailrec
    def next(x: Int, factor: Int, xs: IndexedSeq[BigInt]): Int = {
      val leq = factor * xs(x) <= xs.last
      if (leq) next(x + 1, factor, xs)
      else x
    }

    @tailrec
    def loop(i: Int, j: Int, k: Int, xs: IndexedSeq[BigInt]): IndexedSeq[BigInt] = {
      if (xs.size < n) {
        val a = next(i, 2, xs)
        val b = next(j, 3, xs)
        val c = next(k, 5, xs)
        val m = Seq(2 * xs(a), 3 * xs(b), 5 * xs(c)).min

        val x = a + (if (2 * xs(a) == m) 1 else 0)
        val y = b + (if (3 * xs(b) == m) 1 else 0)
        val z = c + (if (5 * xs(c) == m) 1 else 0)

        loop(x, y, z, xs :+ m)
      } else xs
    }

    loop(0, 0, 0, IndexedSeq(BigInt(1)))
  }

  /*
   * Square root by Newton's method
   *
   * ANSWER: newGuess = guess - (guess² - num) / 2 x guess
   * https://www.youtube.com/watch?v=tUFzOLDuvaE
   */
  def sqrt(num: Int, epsilon: Double = 0.001): Double = {
    Iterator.iterate((num * 1d, Double.PositiveInfinity)) { case (guess, _) =>
      val newGuess = guess - ((guess * guess - num) / (2 * guess))
      (newGuess, math.abs(newGuess - guess))
    }
      .dropWhile(_._2 >= epsilon)
      .take(1)
      .map(_._1)
      .next()
  }

  /*
   * x^y
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/primitives/package.scala
   */

  /*
   * Sum two large positive integers represented as strings.
   *
   * ANSWER: Recall grade school arithmetic. To sum two numbers, we line them up on the right, and add individual
   * digits, plus the carry over. If one of the numbers is shorter, we assume a zero for that position.
   *
   * See addition.png.
   */
  def largeSum(x: String, y: String): String = {
    @tailrec
    def loop(result: ListBuffer[Int], i: Int, j: Int, carry: Int): String = {
      if (i < 0 && j < 0) {
        val a = if (carry == 1) 1 +=: result
        else result

        a.mkString
      } else {
        val a = x.lift(i).map(_.asDigit).getOrElse(0)
        val b = y.lift(j).map(_.asDigit).getOrElse(0)

        val sum = a + b + carry
        if (sum >= 10) loop((sum - 10) +=: result, i - 1, j - 1, 1)
        else loop(sum +=: result, i - 1, j - 1, 0)
      }
    }

    loop(ListBuffer.empty[Int], x.length - 1, y.length - 1, 0)
  }

  /*
   * Multiply two big numbers.
   *
   * See https://github.com/asarkar/algorithms-design-analysis/blob/master/karatsuba/src/main/scala/org/asarkar/Karatsuba.scala
   */

  /*
   * The area of a circle is defined as πr^2. Estimate π to 3 decimal places using a Monte Carlo method.
   * Hint: The basic equation of a circle is x2 + y2 = r2.
   *
   * ANSWER: Imagine a circle of radius 1 enclosed in a square. Each side of the square is of length 2 (equal to the
   * diameter of the circle). Area of the circle C = πr^2 = π. Area of the square S is 2x2 = 4. If we take
   * the ratio R of C and S, we get π/4. Therefore, π = 4R.
   * Assuming the center (0,0) at the center of the circle, the lower left corner of the square is (-1,-1) and the
   * upper right corner is (1,1). We randomly generate 2D points in this range; if the distance of a point from the
   * center is less than 1, it is inside the circle, otherwise, it's outside.
   * Experiments show that the number of iterations required is approximately 10 raised to the precision desired.
   */
  def pi(actual: Double, error: Double): Double = {
    Iterator.iterate((0, 0, Double.MaxValue)) { case (numPointsInCircle, numTotalPoints, _) =>
      val x = ThreadLocalRandom.current.nextDouble(-1d, 1.0001)
      val y = ThreadLocalRandom.current.nextDouble(-1d, 1.0001)
      val d = x * x + y * y
      // points on the circumference are considered outside
      val a = numPointsInCircle + (if (d < 1d) 1 else 0)
      val b = numTotalPoints + 1
      val currentVal = (4d * a) / b
      val estimate = math.abs(currentVal - actual)
      (numPointsInCircle + (if (d < 1d) 1 else 0), numTotalPoints + 1, estimate)
    }
      .dropWhile(_._3 > error)
      .take(1)
      .map { x =>
        println(s"Calculated PI up to $error precision in ${x._2} iterations")
        (4d * x._1) / x._2
      }
      .next()
  }

  /*
   * Given a stream of elements too large to store in memory, pick a random element from the stream with uniform
   * probability.
   *
   * ANSWER: We will prove that every element is has a 1/n probability to be chosen, where n is the number of elements
   * seen so far.
   * For n = 1, we pick the first element.
   * After n iterations, the probability that the i-th element was chosen, and not replaced by any of the [i+1..n]
   * elements is the product of the probabilities of choosing the i-th element, and not choosing any of the remaining
   * elements (product since these probabilities are independent).
   * 1/i + (1 - (1/i+1)) + (1 - (1/i+2)) + ... + (1 - 1/n)
   * = 1/i + i/(i+1) + (i+1)/(i+2) + ... + (n-1)/n
   * = 1/n
   *
   * c.f. https://en.wikipedia.org/wiki/Reservoir_sampling
   * https://kapilddatascience.wordpress.com/2015/06/11/how-to-randomly-pick-k-elements-from-an-infinite-stream-with-equal-probability-a-k-a-reservoir-sampling/
   */
  def randomNumFromStream(numbers: Iterator[Int]): Iterator[Int] = {
    numbers
      .scanLeft((0, -1)) { case ((count, rand), x) =>
        if (count == 0) (count + 1, x)
        else if (new Random().nextInt(count) == count - 1) (count + 1, x)
        else (count + 1, rand)
      }
      .drop(1)
      .map(_._2)
  }
}

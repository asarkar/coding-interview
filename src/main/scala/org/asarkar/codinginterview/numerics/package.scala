package org.asarkar.codinginterview

import java.util.concurrent.ThreadLocalRandom

import org.asarkar.codinginterview.stacksnqueues.allPrimes

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

package object numerics {
  /*
   * Using a function rand5() that returns an integer from 1 to 5 (inclusive) with uniform probability, implement a
   * function rand7() that returns an integer from 1 to 7 (inclusive).
   *
   * ANSWER: See https://github.com/asarkar/adm/blob/master/src/main/scala/org/asarkar/adm/combinatorial/package.scala
   */

  /*
   * Assume you have access to a function toss_biased() which returns 0 or 1 with a probability that's not 50-50
   * (but also not 0-100 or 100-0). You do not know the bias of the coin.
   *
   * Write a function to simulate an unbiased coin toss.
   *
   * ANSWER: The solution to this can be attributed to mathematician John von Neumann.
   *
   * Let's assume that the probability of getting a heads is 0.7 and probability of getting a tails is 0.3.
   * The probability of flipping a HT is P(heads) x P(tails) = 0.7 x 0.3 = .21
   * The probability of flipping a TH is P(tails) x P(heads) = 0.3 x 0.7 = .21
   *
   * Thus, we throw the coin twice. If it’s TH, we say it's T. If it’s HT, we say it's H. If it's either HH or TT,
   * we repeat the process.
   */

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
   * 1/i x (1 - (1/i+1)) x (1 - (1/i+2)) x ... x (1 - 1/n)
   * = 1/i x i/(i+1) x (i+1)/(i+2) x ... x (n-1)/n
   * = 1/n
   *
   * Thus, at the i-th iteration, we generate a random number in [0, i). If the randomly generated number is equal to
   * i - 1 (the largest possible value in the range, probability 1/i), we select the current element. Otherwise, we
   * retain the element selected in one of the previous iterations.
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

  /*
   * A number is considered perfect if its digits sum up to exactly 10.
   * Given a positive integer n, return the n-th perfect number.
   * For example, given 1, you should return 19. Given 2, you should return 28.
   *
   * ANSWER: By observation, the n-th perfect number is the string "n" + (10 - sum of the digits of n).
   */
  def perfectNum(n: Int): Int = {
    val rest = 10 - n.toString
      .map(_.asDigit)
      .sum

    s"$n$rest".toInt
  }

  /*
   * Using a function rand7() that returns an integer from 1 to 7 (inclusive) with uniform probability, implement a
   * function rand5() that returns an integer from 1 to 5 (inclusive).
   *
   * ANSWER:
   * do {
   *   r = rand7
   * } while r > 5
   * return r;
   */

  /*
   * Suppose you have a multiplication table that is N by N. That is, a 2D array where the value at the i-th row and
   * j-th column is (i + 1) * (j + 1) (if 0-indexed) or i * j (if 1-indexed).
   *
   * Given integers N and X, write a function that returns the number of times X appears as a value in an N by N
   * multiplication table.
   *
   * For example, given N = 6 and X = 12, you should return 4, since the multiplication table looks like this:
   * | 1 | 2 | 3 | 4 | 5 | 6 |
   * | 2 | 4 | 6 | 8 | 10 | 12 |
   * | 3 | 6 | 9 | 12 | 15 | 18 |
   * | 4 | 8 | 12 | 16 | 20 | 24 |
   * | 5 | 10 | 15 | 20 | 25 | 30 |
   * | 6 | 12 | 18 | 24 | 30 | 36 |
   *
   * And there are 4 12's in the table.
   *
   * ANSWER: We simply need to find all the divisors of x. If the (divisor, quotient) pair is within the bounds of
   * the table, by symmetry the (quotient, divisor) pair is within the bounds too, so each pair is counted twice.
   * The exception is when both items are equal, like (10, 10).
   * We don't need to check beyond the square root of x, because if x = a * b, and if both 'a' and 'b' were greater
   * than the square root of n, then a * b would be greater than x. We simply need to find the smaller of 'a' and 'b',
   * since the other one would be greater than the square root.
   */
  def divisors(n: Int, x: Int): Int = {
    (1 to math.sqrt(x).toInt)
      .withFilter(x % _ == 0)
      .map(i => (i, x / i))
      .filter(a => a._1 <= n && a._2 <= n)
      .map(a => if (a._1 == a._2) 1 else 2)
      .sum
  }

  /*
   * Given an even number (greater than 2), return two prime numbers whose sum will be equal to the given number.
   * A solution will always exist. See Goldbach’s conjecture (https://en.wikipedia.org/wiki/Goldbach%27s_conjecture).
   *
   * Example:
   * Input: 4
   * Output: 2 + 2 = 4
   *
   * If there are more than one solution possible, return the lexicographically smaller solution.
   * If [a, b] is one solution with a <= b, and [c, d] is another solution with c <= d, then
   * [a, b] < [c, d] if a < c OR a == c AND b < d.
   *
   * ANSWER: allPrimes returns the primes in sorted order. Thus, lexicographically smaller solution is a no brainer.
   */
  def primeAddends(n: Int): (Int, Int) = {
    val primes = allPrimes(n)

    val x = primes
      .dropWhile(i => !primes.contains(n - i))
      .head
    (x, n - x)
  }

  /*
   * Given a positive integer, check if the number is prime or not.  A prime is a natural number greater than 1 that
   * has no positive divisors other than 1 and itself.
   *
   * ANSWER:
   * Naive: Iterate through all numbers from 2 to n - 1 and if it divides n, return false.
   *
   * Better: Instead of checking till n, we can check till √n because a larger factor of n must be a multiple of
   * smaller factor that has been already checked (see detailed explanation with Sieve of Eratosthenes).
   *
   * Even better: The algorithm can be improved further by observing that all primes are of the form 6k ± 1, with the
   * exception of 2 and 3. This is because all integers can be expressed as (6k + i) for some integer k and for
   * i = -1, 0, 1, 2, 3, or 4; 2 divides (6k + 0), (6k + 2), (6k + 4); and 3 divides (6k + 3). So, a more efficient
   * method is to test if n is divisible by 2 or 3, then to check through all the numbers of the form 6k ± 1 ≤ √n.
   * This is 3 times as fast as testing all numbers till √n.
   */
  def isPrime(n: Int): Boolean = {
    if (n <= 1) false
    else if (n <= 3) true
    else if (n % 2 == 0 || n % 3 == 0) false
    else {
      !Iterator.from(5, 6)
        .takeWhile(i => i * i <= n)
        .exists(i => n % i == 0 || n % (i + 2) == 0) // 6k ± 1 (5, 7) for k = 1, (11, 13) for k = 2...
    }
  }

  /*
   * You have n fair coins and you flip them all at the same time. Any that come up tails you set aside. The ones that
   * come up heads you flip again. How many rounds do you expect to play before only one coin remains?
   *
   * Write a function that, given n, returns the number of rounds you'd expect to play until one coin remains.
   *
   * ANSWER: At each round, the number of coins is halved. If the number of coins n is odd, we consider ⌈x/2⌉ coins
   * remain.
   * For example, n = 7.
   * Round 1: 4 coins
   * Round 2: 2 coins
   * Round 3: 1 coin
  */
  def numRound(n: Int): Int = math.ceil(math.log(n) / math.log(2)).toInt
}

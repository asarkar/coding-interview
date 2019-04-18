package org.asarkar.codinginterview

package object combinatorial {
  /*
   * All subsets of a set
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/recursion/package.scala
   */

  /*
   * Power set
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/recursion/package.scala
   */

  /*
   * Using a function rand5() that returns an integer from 1 to 5 (inclusive) with uniform probability, implement a
   * function rand7() that returns an integer from 1 to 7 (inclusive).
   *
   * ANSWER: See https://github.com/asarkar/adm/blob/master/src/main/scala/org/asarkar/adm/combinatorial/package.scala
   */

  /*
   * Given a function that generates perfectly random numbers between 1 and k (inclusive), where k is an input, write
   * a function that shuffles a deck of cards represented as an array using only swaps.
   * It should run in O(N) time.
   * Hint: Make sure each one of the 52! permutations of the deck is equally likely.
   *
   * ANSWER: Use Fisher-Yates shuffle.
   * for i from 0 to n - 2 do
   *   j <- random integer such that i <= j < n
   *   exchange a[i] and a[j]
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
}

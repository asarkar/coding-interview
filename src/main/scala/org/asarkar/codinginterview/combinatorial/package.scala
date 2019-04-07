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
}

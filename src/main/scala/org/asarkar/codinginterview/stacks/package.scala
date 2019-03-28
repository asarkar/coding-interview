package org.asarkar.codinginterview

package object stacks {
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
}

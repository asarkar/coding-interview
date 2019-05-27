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
}

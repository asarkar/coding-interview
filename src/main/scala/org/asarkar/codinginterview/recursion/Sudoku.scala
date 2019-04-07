package org.asarkar.codinginterview.recursion

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
 * Sudoku is a puzzle where you're given a partially-filled 9 by 9 grid with digits. The objective is to fill the grid
 * with the constraint that every row, column, and box (3 by 3 subgrid) must contain all of the digits from 1 to 9.
 * Implement an efficient sudoku solver.
 *
 * ANSWER: In pseudocode, our strategy is:
 *
 * Find row, col of an unassigned cell
 * If there is none, return true
 *
 * for digit in 1 to 9
 *   if there is no conflict for digit at row, col
 *     assign digit to row, col and recursively fill in rest of the board
 *     if successful, return true
 *     else unassign digit and try next digit
 *   else try next digit
 * if all digits have been tried and nothing worked, return false to trigger backtracking
 *
 * Another way of solving this problem is by reducing it to a vertex coloring problem, where each cell is a vertex in
 * a graph, and edges exist between it and every other cell in the same row, column, and box (3x3 grid). We then
 * attempt to assign colors to each vertex such that no two adjacent vertices share the same color.
 */
class Sudoku(private val board: Array[Array[Int]]) {
  private val available = ListBuffer.empty[(Int, Int)]
  private val rowMap = new mutable.HashMap[Int, mutable.Set[Int]] with mutable.MultiMap[Int, Int]
  private val colMap = new mutable.HashMap[Int, mutable.Set[Int]] with mutable.MultiMap[Int, Int]

  for (row <- board.indices; col <- board(row).indices) {
    val i = board(row)(col)
    if (i < 1) available.append((row, col))
    else {
      rowMap.addBinding(row, i)
      colMap.addBinding(col, i)
    }
  }

  private def hasConflict(cell: (Int, Int), digit: Int): Boolean = {
    val startRow = (cell._1 / 3) * 3
    val endRow = startRow + 3
    val startCol = (cell._2 / 3) * 3
    val endCol = startCol + 3

    rowMap(cell._1).contains(digit) ||
      colMap(cell._2).contains(digit) ||
      (startRow until endRow)
        .flatMap(r => (startCol until endCol).map((r, _)))
        .exists { case (r, c) => board(r)(c) == digit }
  }

  private def assign(cell: (Int, Int), digit: Int): Unit = {
    board(cell._1)(cell._2) = digit
    rowMap(cell._1).add(digit)
    colMap(cell._2).add(digit)
  }

  private def unassign(cell: (Int, Int), digit: Int): Unit = {
    board(cell._1)(cell._2) = 0
    rowMap(cell._1).remove(digit)
    colMap(cell._2).remove(digit)
  }

  private def attempt(digit: Int): Boolean = {
    if (available.isEmpty) true
    else if (digit > 9) false
    else {
      val avl = available.head

      if (hasConflict(avl, digit)) {
        attempt(digit + 1)
      } else {
        available.remove(0)
        assign(avl, digit)
        if (attempt(1)) true
        else {
          available.prepend(avl)
          unassign(avl, digit)
          attempt(digit + 1)
        }
      }
    }
  }

  def solve(): Boolean = attempt(1)

  def show(): Unit = println(board.map(_.map(i => if (i == 0) "-" else s"$i").mkString(" ")).mkString(System.lineSeparator()))
}

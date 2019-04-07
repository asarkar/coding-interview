package org.asarkar.codinginterview.arrays

import scala.util.Random

class Minesweeper(val size: Int, val numMines: Int) {
  private val board = Array.ofDim[Int](size, size)
  private val end = size * size - 1

  def numMinesAround(row: Int, col: Int): Int = board(row)(col)

  import Minesweeper.Mine

  def show(): Unit = println(board.map(_.mkString(" ")).mkString(System.lineSeparator()))
  (1 to numMines)
    .foldLeft(Seq.empty[Int]) { (mines, _) =>
      val mine = randWithExclusion(0, end, mines)
      val (row, col) = (mine / size, mine % size)

      assert(!hasMine(row, col), s"[$row][$col] already has a mine")
      board(row)(col) = Mine
      mines :+ mine
    }
    .foreach { mine =>
      val (row, col) = (mine / size, mine % size)
      neighbors(row, col)
        .foreach(x => board(x._1)(x._2) += 1)
    }

  /*
   * Consider the range broken up into start..ex₁..ex₂..exₖ..end. If the random number 'r' falls in [start, ex₁), we
   * are golden. Otherwise, we increment 'r' by 1, and check if it falls in [ex₁, ex₂). We keep doing this, until
   * we find a 'r' that falls in the range between two exclusions. in the worst case, we have to increment 'r'
   * 'k' times.
   *
   * Another option is to use a discrete probability distribution with the probabilities of the excluded positions as
   * zero and all others as one. c.f. Apache Commons Math EnumeratedDistribution.
   */
  private def randWithExclusion(start: Int, end: Int, exclusion: Seq[Int]): Int = {
    val r = start + Random.nextInt(end - start + 1 - exclusion.size)

    if (exclusion.isEmpty) r
    else {
      exclusion
        .sorted
        .zipWithIndex
        .map(x => (x._1, r + x._2))
        .dropWhile(x => x._2 >= x._1)
        .take(1)
        .map(_._2)
        .headOption
        .getOrElse(r + exclusion.size)
    }
  }

  private def neighbors(row: Int, col: Int): Seq[(Int, Int)] = {
    val candidates = for {
      r <- row - 1 to row + 1 if board.isDefinedAt(r)
      c <- col - 1 to col + 1 if board(r).isDefinedAt(c)
    } yield (r, c)

    candidates
      .filterNot(x => hasMine(x._1, x._2))
  }

  def hasMine(row: Int, col: Int): Boolean = board(row)(col) == Mine

  // TODO: Implement expansion
}

object Minesweeper {
  val Mine = 9

  def main(args: Array[String]): Unit = {
    new Minesweeper(8, 10).show()
  }
}

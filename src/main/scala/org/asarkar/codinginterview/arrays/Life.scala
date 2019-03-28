package org.asarkar.codinginterview.arrays

/*
 * Conway's Game of Life takes place on an infinite two-dimensional board of square cells. Each cell is either dead or
 * alive, and at each tick, the following rules apply:
 *
 * Any live cell with less than two live neighbours dies.
 * Any live cell with two or three live neighbours remains living.
 * Any live cell with more than three live neighbours dies.
 * Any dead cell with exactly three live neighbours becomes a live cell.
 *
 * A cell neighbours another cell if it is horizontally, vertically, or diagonally adjacent.
 *
 * Implement Conway's Game of Life. It should be able to be initialized with a starting list of live cell coordinates
 * and the number of steps it should run for. Once initialized, it should print out the board state at each step.
 * Since it's an infinite board, print out only the relevant coordinates, i.e. from the top-leftmost live cell to
 * bottom-rightmost live cell.
 *
 * You can represent a live cell with an asterisk (*) and a dead cell with a dot (.).
 *
 * ANSWER: Since the board is infinite, we store the coordinates of the maximum and the minimum live cells. We also
 * store the boundaries as determined by the coordinates of the maximum and the minimum live cells. During a step
 * (a.k.a. 'tick'), we allow the neighbors of the maximum and the minimum live cells to come alive.
 *
 * This class is not thread-safe.
 * Time and space complexity are proportional to the number of live cells.
 *
 * A more sophisticated algorithm called Hashlife exists.
 */
class Life(val liveCells: Seq[(Int, Int)], val maxSteps: Int) extends Iterator[Set[(Int, Int)]] {
  private var steps = 0
  private var top = 0
  private var bottom = 0
  private var left = 0
  private var right = 0
  private var live = Set(liveCells: _*)

  live
    .foreach(updateBoundaries)

  private def updateBoundaries(x: (Int, Int)): Unit = {
    top = math.min(top, x._1)
    bottom = math.max(bottom, x._1)
    left = math.min(left, x._2)
    right = math.max(right, x._2)
  }

  private def numNeighborsAlive(x: Int, y: Int): Int = Seq(
    (x - 1, y),
    (x + 1, y),
    (x, y - 1),
    (x, y + 1),
    (x - 1, y - 1),
    (x - 1, y + 1),
    (x + 1, y - 1),
    (x + 1, y + 1)
  )
    .count(i => live.contains((i._1, i._2)))

  override def hasNext: Boolean = steps < maxSteps

  override def next(): Set[(Int, Int)] = {
    if (!hasNext) throw new NoSuchElementException("next on empty iterator")

    val tempLive = collection.mutable.Set.empty[(Int, Int)]
    for (
      x <- math.max(0, top - 1) to math.min(Int.MaxValue, bottom + 1);
      y <- math.max(0, left - 1) to math.min(Int.MaxValue, right + 1)
    ) {
      val alive = live.contains((x, y))
      val aliveNeighbors = numNeighborsAlive(x, y)

      if (aliveNeighbors == 3 || (alive && (aliveNeighbors == 2))) {
        tempLive += ((x, y))
        updateBoundaries((x, y))
      }
    }

    live = tempLive.toSet
    steps += 1
    live
  }

  def show(): Unit = {
    val buffer = new StringBuilder()
    for (x <- top to bottom) {
      for (y <- left to right) {
        if (y > left) buffer.append(' ')
        buffer.append(if (live.contains((x, y))) '*' else '.')
        if (y < right) buffer.append(' ')
      }
      if (x < bottom) buffer.append(System.lineSeparator)
    }
    println(buffer.toString())
  }
}

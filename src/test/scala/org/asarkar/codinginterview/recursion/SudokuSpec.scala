package org.asarkar.codinginterview.recursion

import org.jsoup.Jsoup
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.collection.JavaConverters._

class SudokuSpec extends FlatSpec {
  "sudoku" should "be solved" in {
    val boards = Seq(
      Array(
        Array(5, 3, 0, 0, 7, 0, 0, 0, 0),
        Array(6, 0, 0, 1, 9, 5, 0, 0, 0),
        Array(0, 9, 8, 0, 0, 0, 0, 6, 0),
        Array(8, 0, 0, 0, 6, 0, 0, 0, 3),
        Array(4, 0, 0, 8, 0, 3, 0, 0, 1),
        Array(7, 0, 0, 0, 2, 0, 0, 0, 6),
        Array(0, 6, 0, 0, 0, 0, 2, 8, 0),
        Array(0, 0, 0, 4, 1, 9, 0, 0, 5),
        Array(0, 0, 0, 0, 8, 0, 0, 7, 9)
      ),
      Array(
        Array(3, 0, 6, 5, 0, 8, 4, 0, 0),
        Array(5, 2, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 8, 7, 0, 0, 0, 0, 3, 1),
        Array(0, 0, 3, 0, 1, 0, 0, 8, 0),
        Array(9, 0, 0, 8, 6, 3, 0, 0, 5),
        Array(0, 5, 0, 0, 9, 0, 6, 0, 0),
        Array(1, 3, 0, 0, 0, 0, 2, 5, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 7, 4),
        Array(0, 0, 5, 2, 0, 6, 3, 0, 0)
      )
    )

    boards.foreach { board =>
      val sudoku = new Sudoku(board)
      sudoku.solve()
      val requestBody = board.indices.flatMap(r => board.indices.map(c =>
        Seq(r / 3, r % 3, c / 3, c % 3).mkString -> board(r)(c).toString
      )).toMap + ("submitted" -> "y")
      val h2 = Jsoup.connect("http://www.birot.hu/sudoku.php")
        .timeout(10 * 1000)
        .data(requestBody.asJava)
        .post()
        .select("h2")
        .asScala
        .head
      // Number of solutions found: ? .
      val numSolutions = h2.text().split(':').last.filter(_.isDigit).head.asDigit
      withClue("Number of solutions: ") {
        numSolutions shouldBe 1
      }
    }
  }
}

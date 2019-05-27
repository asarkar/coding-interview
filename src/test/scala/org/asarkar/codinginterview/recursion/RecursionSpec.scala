package org.asarkar.codinginterview.recursion

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class RecursionSpec extends FlatSpec with TableDrivenPropertyChecks {
  "recursion" should "find the longest absolute path to a file" in {
    val data = Table(
      ("file system", "longest file path length"),
      ("dir\n\tsubdir1\n\tsubdir2\n\t\tfile.ext", 20),
      ("dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext", 32),
      ("dir\n\tsubdir1\n\tsubdir2", 0)
    )
    forAll(data) { (fileSys, len) =>
      longestFilePathLength(fileSys) shouldBe len
    }
  }

  it should "break a string into dictionary words" in {
    val data = Table(
      ("dict", "str", "words"),
      (
        Set("this", "th", "is", "famous", "word", "break", "b", "r", "e", "a", "k", "br", "bre", "brea", "ak",
          "problem"
        ),
        "wordbreakproblem",
        "word break problem"
      ),
      (
        Set("this", "th", "is", "famous", "word", "break", "b", "r", "e", "a", "br", "bre", "brea",
          "problem"
        ),
        "wordbreakproblem",
        "word break problem"
      ),
      (Set("quick", "brown", "the", "fox"), "thequickbrownfox", "the quick brown fox"),
      (Set("bed", "bath", "bedbath", "and", "beyond"), "bedbathandbeyond", "bedbath and beyond"),
      (Set("a", "b", "c", "ab", "bc", "abc"), "abcd", ""),
    )

    forAll(data) { (dict, str, words) =>
      wordBreak(str, dict) shouldBe words
    }
  }

  it should "return the minimum number of steps required to reach the end coordinate" in {
    minSteps(IndexedSeq(
      IndexedSeq(false, false, false, false),
      IndexedSeq(true, true, false, true),
      IndexedSeq(false, false, false, false),
      IndexedSeq(false, false, false, false)
    ),
      (3, 0),
      (0, 0)
    ) shouldBe 7
  }

  it should "generate all valid combinations of n-pairs of parentheses" in {
    combineParenthesis(0) shouldBe empty
    combineParenthesis(-1) shouldBe empty
    combineParenthesis(1) should contain theSameElementsAs Seq("()")
    combineParenthesis(2) should contain theSameElementsAs Seq("(())", "()()")
    combineParenthesis(3) should contain theSameElementsAs Seq("((()))", "(()())", "(())()", "()(())", "()()()")
  }

  it should "determine if the graph coloring problem is solvable for the given graph and k colors" in {
    val g = IndexedSeq(
      IndexedSeq(false, true, false, true),
      IndexedSeq(true, false, true, true),
      IndexedSeq(false, true, false, true),
      IndexedSeq(true, true, true, false)
    )
    isColorable(g, 3) shouldBe true
  }

  it should "implement integer multiplication" in {
    val data = Table(
      ("x", "y", "pdt"),
      (2, 3, 8d),
      (2, -3, 1d / 8),
      (2, 0, 1d)
    )

    forAll(data) { (x, y, pdt) =>
      fastPow(x, y) shouldBe (pdt +- 0.01d)
    }
  }

  it should "find if the word exists in the grid" in {
    val data = Table(
      ("grid", "word", "found"),
      (IndexedSeq(
        IndexedSeq('A', 'B', 'C', 'E'),
        IndexedSeq('S', 'F', 'C', 'S'),
        IndexedSeq('A', 'D', 'E', 'E')
      ), "ABCCED", true),
      (IndexedSeq(
        IndexedSeq('A', 'B', 'C', 'E'),
        IndexedSeq('S', 'F', 'C', 'S'),
        IndexedSeq('A', 'D', 'E', 'E')
      ), "SEE", true),
      (IndexedSeq(
        IndexedSeq('A', 'B', 'C', 'E'),
        IndexedSeq('S', 'F', 'C', 'S'),
        IndexedSeq('A', 'D', 'E', 'E')
      ), "ABCB", false),
      (IndexedSeq(
        IndexedSeq('A')
      ), "A", true),
      (IndexedSeq(
        IndexedSeq('C', 'A', 'A'),
        IndexedSeq('A', 'A', 'A'),
        IndexedSeq('B', 'C', 'D')
      ), "AAB", true),
      (IndexedSeq(
        IndexedSeq('A', 'B', 'C', 'E'),
        IndexedSeq('S', 'F', 'E', 'S'),
        IndexedSeq('A', 'D', 'E', 'E')
      ), "ABCESEEEFS", true)
    )

    forAll(data) { (grid, word, found) =>
      hasWord(grid, word) shouldBe found
    }
  }

  it should "compute a knight's tour" in {
    val tour = knightsTour(8)
    tour should not be empty
    println(tour.map(_.map(i => f"$i%2d").mkString(" ")).mkString(System.lineSeparator()))
    tour.foreach(row => row.foreach { move =>
      move should be >= 1
      move should be <= 64
    })
  }

  it should "compute the minimum and maximum elements in an array using less than 2 * (n - 2) comparisons" in {
    minMax(IndexedSeq(4, 2, 7, 5, -1, 3, 6)) shouldBe(-1, 7)
  }

  it should "compute the total number of unlock patterns of length n" in {
    numUnlockCombinations(1) shouldBe 9
  }

  it should "return all possible letters the number could represent" in {
    allPossibleLetters("23", Map(2 -> ('a' to 'c'), 3 -> ('d' to 'f'))) should contain theSameElementsAs Seq(
      "ad", "ae", "af",
      "bd", "be", "bf",
      "cd", "ce", "cf"
    )
  }

  it should "solve a cryptarithmetic puzzle" in {
    println(cryptarithmeticSoln(Seq("SEND", "MORE", "MONEY")))
  }
}

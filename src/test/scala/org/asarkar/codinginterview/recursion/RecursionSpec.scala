package org.asarkar.codinginterview.recursion

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.OptionValues._

class RecursionSpec extends FlatSpec with TableDrivenPropertyChecks {
  "recursion" should "find the number of ways to decode the given message" in {
    val data = Table(
      ("msg", "n"),
      ("", 1),
      ("1", 1),
      ("12345", 3),
      ("54321", 2),
      ("011", 0)
    )

    forAll(data) { (msg, n) =>
      numWaysToDecode(msg) shouldBe n
    }
  }

  it should "find the longest absolute path to a file" in {
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
}

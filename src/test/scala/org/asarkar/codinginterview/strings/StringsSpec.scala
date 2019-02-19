package org.asarkar.codinginterview.strings

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class StringsSpec extends FlatSpec with TableDrivenPropertyChecks {
  "strings" should "find the longest consecutive character" in {
    val data = Table(
      ("s", "ch", "n"),
      ("", '\u0000', 0),
      ("a", 'a', 1),
      ("aabcddbbbea", 'b', 3),
      ("abcddbbb", 'b', 3),
      ("cbccca", 'c', 3)
    )

    forAll(data) { (s, ch, n) =>
      longestConsecutive(s) shouldBe ((ch, n))
    }
  }

  it should "find the first non-repeating char in a string" in {
    firstNonRepeatingChar("GeeksforGeeks") shouldBe('f', 5)
    firstNonRepeatingChar("GeeksQuiz") shouldBe('G', 0)
  }

  it should "check if edit distance between two strings is one" in {
    isOneAway("gfg", "gf") shouldBe true
  }

  it should "test if a given string is a palindrome" in {
    val data = Table(
      ("s", "palindrome"),
      ("", true),
      ("a", true),
      ("aba", true),
      ("aaaa", true),
      ("abcde", false),
      ("Borrow or rob?", true),
      ("Was it a car or a cat I saw?", true),
      ("Amore, Roma.", true)
    )
  }

  it should "reverse a string" in {
    reverse("hello world") shouldBe "dlrow olleh"
  }

  it should "print all anagrams together" in {
    anagrams1(Seq("the", "rat", "fell", "in", "the", "tar")) should contain theSameElementsAs Seq(Set("rat", "tar"))
  }
}

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

  it should "find the length of the longest substring with at most k distinct char" in {
    longestSubstringWithKDistinctChar("abcba", 2) shouldBe 3
    longestSubstringWithKDistinctChar("eceba", 2) shouldBe 3
    longestSubstringWithKDistinctChar("aa", 2) shouldBe 2
  }

  it should "implement run-length encoding" in {
    runLengthEncoding("AAAABBBCCDAA") shouldBe "4A3B2C1D2A"
    runLengthEncoding("A") shouldBe "1A"
  }

  it should "implement run-length decoding" in {
    runLengthDecoding("4A3B2C1D2A") shouldBe "AAAABBBCCDAA"
    runLengthDecoding("1A") shouldBe "A"
  }

  it should "determine if two strings are isomorphic" in {
    val data = Table(
      ("s1", "s2", "isomorphic"),
      ("add", "egg", true),
      ("a", "b", true),
      ("", "", true),
      ("foo", "bar", false),
      ("abcabc", "xbexyz", false),
      ("abcd", "aabb", false),
      ("abcabc", "xyzxyz", true),
      ("aba", "baa", false)
    )

    forAll(data) { (s1, s2, isomorphic) =>
      isIsomorphic(s1, s2) shouldBe isomorphic
    }
  }

  it should "find the longest palindromic substring" in {
    longestPalindromicSubstr("aabcdcb") shouldBe "bcdcb"
    longestPalindromicSubstr("bananas") shouldBe "anana"
    longestPalindromicSubstr("ABABABA") shouldBe "ABABABA"
  }

  it should "shorten URLs" in {
    val url = "https://www.scala-lang.org/files/archive/api/current/scala/"
    val u1 = urlShortener1(url)
    val u2 = urlShortener2(url)

    u1.length shouldBe 6
    u2.length shouldBe 6
    u2.forall(Character.isLowerCase) shouldBe true
  }
}

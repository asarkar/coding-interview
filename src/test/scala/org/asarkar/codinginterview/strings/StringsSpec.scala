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

  it should "all pairs of distinct indices that form a palindrome" in {
    palindromePairs(Seq("abcd", "dcba", "lls", "s", "sssll")) should contain theSameElementsAs
      Seq((0, 1), (1, 0), (3, 2), (2, 4))
    palindromePairs(Seq("bat", "tab", "cat")) should contain theSameElementsAs
      Seq((0, 1), (1, 0))
  }

  it should "output a string in zigzag pattern" in {
    zigZag("PAYPALISHIRING", 3) shouldBe "PAHNAPLSIIGYIR"
    zigZag("PAYPALISHIRING", 4) shouldBe "PINALSIGYAHRPI"
  }

  it should "find the lexicographically minimum string rotation" in {
    lexicographicallyMinRotation("baabbaba") shouldBe "aabbabab"
    lexicographicallyMinRotation("BCABDADAB") shouldBe "ABBCABDAD"
  }

  it should "Maximum XOR of two Numbers in an array" in {
    maxXor(IndexedSeq(4, 6, 7)) shouldBe 3
    maxXor(IndexedSeq(3, 10, 5, 25, 2, 8)) shouldBe 28
  }

  it should "find all the starting indices of the pattern in the string" in {
    findAllOccurrences("abracadabra", "abr") should contain theSameElementsInOrderAs Seq(0, 7)
  }

  it should "determine the winning letters" in {
    winningLettersForGhost(Seq("cat", "calf", "dog", "bear")) should contain theSameElementsAs Set('b')
  }

  it should "find the minimum window" in {
    val data = Table(
      ("s", "t", "min"),
      ("ADOBECODEBANC", "ABC", "BANC"),
      ("aaaaaaaaaaaabbbbbcdd", "abcdd", "abbbbbcdd"),
      ("cabwefgewcwaefgcf", "cae", "cwae"),
      ("ab", "A", ""),
      ("ab", "a", "a"),
      ("a", "aa", ""),
      ("aa", "aa", "aa"),
      ("acbbaca", "aba", "baca")
    )

    forAll(data) { (s, t, min) =>
      minWindow(s, t) shouldBe min
    }
  }

  it should "determine if a string can be made palindrome by deleting one char" in {
    validPalindrome("aba") shouldBe true
    validPalindrome("abca") shouldBe true
    validPalindrome("aguokepatgbnvfqmgmlcupuufxoohdfpgjdmysgvhmvffcnqxjjxqncffvmhvgsymdjgpfdhooxfuupuculmgmqfvnbgtapekouga") shouldBe true
  }

  it should "check whether a given string represents a number" in {
    val data = Table(
      ("s", "valid"),
      (".-1", false),
      ("e9", false),
      ("0", true),
      ("3.", true),
      ("2e0", true),
      ("0.8", true),
      ("-1.", true),
      ("0..", false),
      ("6e6.5.", false),
      ("6e+6", true),
      ("10", true),
      ("-10", true),
      ("10.1", true),
      ("-10.1", true),
      ("1e5", true),
      ("a", false),
      ("x 1", false),
      ("x 1", false),
      ("a -2", false),
      ("-", false),
      (" -90e3   ", true),
      ("1e", false),
      ("53.5e93", true),
      ("--6", false),
      ("+-3", false),
      ("95a54e53", false)
    )

    forAll(data) { (s, valid) =>
      isValidNumber(s) shouldBe valid
    }
  }
}

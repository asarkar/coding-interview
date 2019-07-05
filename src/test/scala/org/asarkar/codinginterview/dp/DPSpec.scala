package org.asarkar.codinginterview.dp

import java.util.regex.PatternSyntaxException

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class DPSpec extends FlatSpec with TableDrivenPropertyChecks {
  "dp" should "find the number of subsets of the given array that sum to k" in {
    numSubsetsOfSumK(IndexedSeq(2, 4, 6, 10), 16) shouldBe 2
  }

  it should "buy/sell stocks with n transactions to maximize profit" in {
    stocks3(IndexedSeq(2, 5, 7, 1, 4, 3, 1, 3), 3) shouldBe 10
  }

  it should "returns the largest sum of non-adjacent numbers" in {
    largestNonAdjacentSum(IndexedSeq(2, 4, 6, 2, 5)) shouldBe 13
    largestNonAdjacentSum(IndexedSeq(5, 1, 1, 5)) shouldBe 10
    largestNonAdjacentSum(IndexedSeq(5, 5, 10, 40, 50, 35)) shouldBe 80
  }

  it should "compute the minimum cost of coloring houses" in {
    minCostOfColoringHouses(IndexedSeq(
      IndexedSeq(4, 0, 3),
      IndexedSeq(8, 3, 8),
      IndexedSeq(4, 5, 0),
      IndexedSeq(3, 4, 4),
      IndexedSeq(8, 8, 0)
    )) shouldBe 9
    minCostOfColoringHouses(IndexedSeq(
      IndexedSeq(1, 2, 3),
      IndexedSeq(4, 100, 100),
      IndexedSeq(6, 2, 4)
    )) shouldBe 8
    minCostOfColoringHouses(IndexedSeq(
      IndexedSeq(7, 5, 10),
      IndexedSeq(3, 6, 1),
      IndexedSeq(8, 7, 4),
      IndexedSeq(6, 2, 9),
      IndexedSeq(1, 4, 7),
      IndexedSeq(2, 3, 6)
    )) shouldBe 18
  }

  it should "check if a regex pattern matches a text" in {
    val data = Table(
      ("pattern", "text", "match"),
      ("ra.", "ray", true),
      ("ra.", "raymond", false),
      (".*at", "chat", true),
      (".*at", "chats", false),
      ("xa*b.c", "xaabyc", true),
      ("a", "aa", false),
      ("a*", "aa", true),
      (".*", "ab", true),
      ("c*a*b*", "aab", true),
      ("mis*is*p*.", "mississippi", false)
    )

    forAll(data) { (`pattern`, text, `match`) =>
      isRegexMatch(`pattern`, text) shouldBe `match`
    }

    an[PatternSyntaxException] should be thrownBy isRegexMatch("*a", "a")
  }

  it should "find the palindrome that can be made by inserting the fewest number of characters" in {
    val data = Table(
      ("word", "palindrome"),
      ("race", "ecarace"),
      ("google", "elgoogle"),
      ("abcda", "abcdcba"),
      ("abcd", "abcdcba"),
      ("ab", "aba"),
      ("aa", "aa")
    )

    forAll(data) { (word, palindrome) =>
      makePalindromeByFewestEdits(word) shouldBe palindrome
    }
  }

  it should "find subset sum" in {
    subsetSum(IndexedSeq(12, 1, 61, 5, 9, 2), 24).sum shouldBe 24
    subsetSum(IndexedSeq(7, 3, 2, 5, 8), 18).sum shouldBe 18
  }

  it should "find the number of ways of starting at the top-left corner and getting to the bottom-right corner" in {
    numWays(2, 2) shouldBe 2
    numWays(5, 5) shouldBe 70
  }

  it should "count the number of unique BST" in {
    numUniqBST(3) shouldBe 5
    numUniqBST(4) shouldBe 14
    numUniqBST(1) shouldBe 1
  }

  it should "find the number of ways to decode the given message" in {
    val data = Table(
      ("msg", "n"),
      ("", 1),
      ("1", 1),
      ("12345", 3),
      ("54321", 2),
      ("011", 0),
      ("0", 0),
      ("10", 1),
      ("12", 2),
      ("01", 0),
      ("226", 3),
      ("100", 0),
      ("101", 1),
      ("230", 0),
      ("012", 0),
      ("27", 1),
      ("301", 0)
    )

    forAll(data) { (msg, n) =>
      numWaysToDecode(msg) shouldBe n
    }
  }

  it should "determine if a string is k-palindrome" in {
    kPalindrome("abcdecba", 1) shouldBe true
    kPalindrome("abcdeca", 2) shouldBe true
    kPalindrome("acdcb", 1) shouldBe false
    kPalindrome("waterrfetawx", 2) shouldBe true
  }

  it should "find the maximum number of coins" in {
    maxCoins(Seq(
      Seq(0, 3, 1, 1),
      Seq(2, 0, 0, 4),
      Seq(1, 5, 3, 1)
    )) shouldBe 12
  }
}

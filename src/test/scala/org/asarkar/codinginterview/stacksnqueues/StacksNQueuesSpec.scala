package org.asarkar.codinginterview.stacksnqueues

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class StacksNQueuesSpec extends FlatSpec with TableDrivenPropertyChecks {
  "stacks" should "merge overlapping intervals" in {
    val data = Table(
      ("ints", "merged"),
      (Seq((1, 4), (4, 5)), Seq((1, 5))),
      (Seq((1, 4), (2, 3)), Seq((1, 4))),
      (Seq((2, 3), (5, 5), (2, 2), (3, 4), (3, 4)), Seq((2, 4), (5, 5)))
    )

    forAll(data) { (ints, merged) =>
      mergeOverlapping(ints) should contain theSameElementsInOrderAs merged
    }
  }

  it should "compute the minimum number of parentheses to be removed" in {
    numParenthesesToRemove("()())()") shouldBe 1
    numParenthesesToRemove(")(") shouldBe 2
  }

  it should "generate all primes smaller than n " in {
    allPrimes(10) should contain theSameElementsAs Seq(2, 3, 5, 7)
    allPrimes(20) should contain theSameElementsAs Seq(2, 3, 5, 7, 11, 13, 17, 19)
  }

  it should "reverse a string word by word" in {
    reverseWords("the sky is blue") shouldBe "blue is sky the"
    reverseWords("  hello world!  ") shouldBe "world! hello"
    reverseWords("a good   example") shouldBe "example good a"
  }

  it should "reverse the words in the string while maintaining the relative order of the delimiters" in {
    reverseWords("hello/world:here", Set('/', ':')) shouldBe "here/world:hello"
    reverseWords("hello/world:here/", Set('/', ':')) shouldBe "here/world:hello/"
    reverseWords("hello//world:here", Set('/', ':')) shouldBe "here//world:hello"
  }
}

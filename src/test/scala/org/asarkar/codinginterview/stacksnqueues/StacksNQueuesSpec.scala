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
}

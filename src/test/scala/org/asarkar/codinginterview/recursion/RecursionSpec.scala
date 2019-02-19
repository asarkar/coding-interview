package org.asarkar.codinginterview.recursion

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

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
      numWays(msg) shouldBe n
    }
  }

  ""
}

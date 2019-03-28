package org.asarkar.codinginterview.searching

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class SearchingSpec extends FlatSpec with TableDrivenPropertyChecks {
  "searching" should "calculate taxes on a given income" in {
    val data = Table(
      ("income", "tax"),
      (5000, 0d),
      (15000, 500d),
      (25000, 2000d),
      (35000, 4500d),
      (45000, 8000d),
      (55000, 12000d)
    )

    val brackets = (0 to 5)
      .map(i => (i * 10000 + 1, i * 0.1d))

    forAll(data) { (income, tax) =>
      taxes(income, brackets) shouldBe (tax +- 0.1d)
    }
  }

  it should "count the number of integers in a range" in {
    countInRange(IndexedSeq(1, 3, 5, 5, 7, 11, 11, 20), 5, 15) shouldBe 5
    countInRange(IndexedSeq(1, 2), 5, 15) shouldBe 0
    countInRange(IndexedSeq.empty[Int], 5, 15) shouldBe 0
  }
}

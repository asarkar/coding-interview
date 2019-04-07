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

  it should "find an element in a sorted rotated array" in {
    val data = Table(
      ("xs", "x", "i"),
      (IndexedSeq(13, 18, 25, 2, 8, 10), 8, 4),
      (IndexedSeq(13, 18, 25, 2, 8, 10), 2, 3),
      (IndexedSeq(25, 2, 8, 10, 13, 18), 8, 2),
      (IndexedSeq(8, 10, 13, 18, 25, 2), 8, 0),
      (IndexedSeq(5, 6, 7, 8, 9, 10, 1, 2, 3), 3, 8),
      (IndexedSeq(5, 6, 7, 8, 9, 10, 1, 2, 3), 30, -1),
      (IndexedSeq(30, 40, 50, 10, 20), 10, 3)
    )

    forAll(data) { (xs, x, i) =>
      searchInSortedRotatedArray(xs, x) shouldBe i
    }
  }
}

package org.asarkar.codinginterview.bits

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class BitsSpec extends FlatSpec with TableDrivenPropertyChecks {
  "bits" should "find the integer that occurs only once" in {
    loneInt(Seq(6, 1, 3, 3, 3, 6, 6), 3) shouldBe 1
    loneInt(Seq(13, 19, 13, 13), 3) shouldBe 19
    loneInt(Seq(3, -4, 3, 3), 3) shouldBe -4
  }

  it should "determine if a given byte-sequence is valid UTF-8 encoding" in {
    val data = Table(
      ("xs", "valid"),
      (Seq(230, 136, 145), true),
      (Seq(250, 145, 145, 145, 145), false),
      (Seq(248, 130, 130, 130), false),
      (Seq(228, 189, 160, 229, 165, 189, 13, 10), true),
      (Seq(197, 130, 1), true),
      (Seq(235, 140, 4), false),
      (Seq(237), false)
    )

    forAll(data) { (xs, valid) =>
      isValidUtf8(xs) shouldBe valid
    }
  }
}

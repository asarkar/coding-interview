package org.asarkar.codinginterview.bits

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class BitsSpec extends FlatSpec {
  "bits" should "find the integer that occurs only once" in {
    loneInt(Seq(6, 1, 3, 3, 3, 6, 6), 3) shouldBe 1
    loneInt(Seq(13, 19, 13, 13), 3) shouldBe 19
    loneInt(Seq(3, -4, 3, 3), 3) shouldBe -4
  }
}

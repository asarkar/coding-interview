package org.asarkar.codinginterview.design

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

class HistoryMapSpec extends FlatSpec {
  "HistoryMap" should "retrieve the value of a key at a particular time" in {
    val hm1 = new HistoryMap()
    hm1.set(1, 1, 0)
    hm1.set(1, 2, 2)
    hm1.get(1, 1).value shouldBe 1
    hm1.get(1, 3).value shouldBe 2
    val hm2 = new HistoryMap()
    hm2.set(1, 1, 5)
    hm2.get(1, 0) shouldBe empty
    hm2.get(1, 10).value shouldBe 1
    val hm3 = new HistoryMap()
    hm3.set(1, 1, 0)
    hm3.set(1, 2, 0)
    hm3.get(1, 0).value shouldBe 2
  }
}

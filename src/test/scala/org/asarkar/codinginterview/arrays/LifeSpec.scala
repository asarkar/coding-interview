package org.asarkar.codinginterview.arrays

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class LifeSpec extends FlatSpec {
  "life" should "go on" in {
    println("Block Pattern")
    val initBlock = Seq(
      (0, 0), (0, 1),
      (1, 0), (1, 1)
    )
    val block = new Life(initBlock, 1)
    block.hasNext shouldBe true
    block.next() should contain theSameElementsAs initBlock
    block.hasNext shouldBe false
    an[NoSuchElementException] should be thrownBy block.next()
    block.show()

    println("Bloat Pattern")
    val initBloat = Seq(
      (0, 0), (0, 1),
      (1, 0), (1, 2),
      (2, 1)
    )
    val bloat = new Life(initBloat, 1)
    bloat.hasNext shouldBe true
    bloat.next() should contain theSameElementsAs initBloat
    bloat.hasNext shouldBe false
    bloat.show()

    println("Blinker Pattern")
    val blinker = new Life(Seq(
      (0, 1),
      (1, 1),
      (2, 1)
    ), 1)
    blinker.show()
    println()
    blinker.hasNext shouldBe true
    blinker.next() should contain theSameElementsAs Seq(
      (1, 0), (1, 1), (1, 2)
    )
    blinker.hasNext shouldBe false
    blinker.show()

    println("Toad Pattern")
    val toad = new Life(Seq(
      (0, 1), (0, 2), (0, 3),
      (1, 0), (1, 1), (1, 2),
      (2, 2)
    ), 1)
    toad.show()
    println()
    toad.hasNext shouldBe true
    toad.next() should contain theSameElementsAs Seq(
      (0, 0), (0, 3),
      (1, 0),
      (2, 2)
    )
    toad.hasNext shouldBe false
    toad.show()
  }
}

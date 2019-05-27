package org.asarkar.codinginterview.combinatorial

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class CombinatorialSpec extends FlatSpec with TableDrivenPropertyChecks {
  "combinatorial" should "generate a random number excluding the given ones" in {
    Seq(0, 1, 3, 4, 7, 8, 9).contains(randomWithExclusion(10, Seq(2, 5, 6))) shouldBe true
  }

  it should "find the next greater permutation" in {
    val data = Table(
      ("xs", "next"),
      (Array(1, 2, 3), Array(1, 3, 2)),
      (Array(1, 3, 2), Array(2, 1, 3)),
      (Array(3, 2, 1), Array(1, 2, 3)),
      (Array(1, 1, 5), Array(1, 5, 1))
    )

    forAll(data) { (xs, next) =>
      nextPermutation(xs)
      xs shouldBe next
    }
  }

  it should "return all possible permutations" in {
    allPermutations(Array(1, 2, 3)) should contain theSameElementsAs Seq(
      Seq(1, 2, 3), Seq(1, 3, 2), Seq(2, 1, 3), Seq(2, 3, 1), Seq(3, 2, 1), Seq(3, 1, 2)
    )
    allPermutations(Array(1, 2)) should contain theSameElementsAs Seq(Seq(1, 2), Seq(2, 1))
    allPermutations(Array(1)) should contain theSameElementsAs Seq(Seq(1))
    allPermutations(Array.empty[Int]) shouldBe empty
  }
}

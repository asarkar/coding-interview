package org.asarkar.codinginterview.graphs

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class GraphsSpec extends FlatSpec with TableDrivenPropertyChecks {
  "graphs" should "determine whether there is a possible arbitrage" in {
    val data = Table(
      ("rates", "arbitrage"),
      (IndexedSeq(
        IndexedSeq(1d, 0.7d, 13.57d),
        IndexedSeq(1.43d, 1d, 9.5d),
        IndexedSeq(0.16d, 0.11d, 1d)
      ), true)
    )

    forAll(data) { (rates, arbitrage) =>
      println(isArbitragePossible(rates))
      isArbitragePossible(rates) shouldBe arbitrage
    }
  }
}

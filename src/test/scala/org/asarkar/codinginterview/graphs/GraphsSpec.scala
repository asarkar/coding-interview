package org.asarkar.codinginterview.graphs

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class GraphsSpec extends FlatSpec {
  "graphs" should "compute an itinerary" in {
    computeItinerary(Seq(
      ("SFO", "HKO"),
      ("YYZ", "SFO"),
      ("YUL", "YYZ"),
      ("HKO", "ORD")
    ), "YUL") should contain theSameElementsInOrderAs Seq("YUL", "YYZ", "SFO", "HKO", "ORD")

    computeItinerary(Seq(
      ("SFO", "COM"),
      ("HKO", "ORD")
    ), "COM") shouldBe empty

    computeItinerary(Seq(
      ("A", "B"),
      ("A", "C"),
      ("B", "C"),
      ("C", "A")
    ), "A") should contain theSameElementsInOrderAs Seq("A", "B", "C", "A", "C")
  }
}

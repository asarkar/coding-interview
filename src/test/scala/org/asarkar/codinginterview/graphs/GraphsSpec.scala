package org.asarkar.codinginterview.graphs

import org.asarkar.codinginterview.arrays.rotateRight
import org.asarkar.codinginterview.strings.findAllOccurrences
import org.scalatest.Matchers._
import org.scalatest.{AppendedClues, FlatSpec}

class GraphsSpec extends FlatSpec with AppendedClues {
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

    computeItinerary(Seq(
      ("MUC", "LHR"),
      ("JFK", "MUC"),
      ("SFO", "SJC"),
      ("LHR", "SFO")
    ), "JFK") should contain theSameElementsInOrderAs Seq("JFK", "MUC", "LHR", "SFO", "SJC")
    computeItinerary(Seq(
      ("JFK", "KUL"),
      ("JFK", "NRT"),
      ("NRT", "JFK")
    ), "JFK") should contain theSameElementsInOrderAs Seq("JFK", "NRT", "JFK", "KUL")
    // Eulerian cycle not applicable for this problem
    //    computeItinerary(Seq(
    //      ("EZE", "AXA"),
    //      ("TIA", "ANU"),
    //      ("ANU", "JFK"),
    //      ("JFK", "ANU"),
    //      ("ANU", "EZE"),
    //      ("TIA", "ANU"),
    //      ("AXA", "TIA"),
    //      ("TIA", "JFK"),
    //      ("ANU", "TIA"),
    //      ("JFK", "TIA"),
    //    ), "JFK") should contain theSameElementsInOrderAs Seq("JFK","ANU","EZE","AXA","TIA","ANU","JFK","TIA","ANU","TIA","JFK")
  }

  it should "find if a undirected graph has a cycle" in {
    hasCycle(Map(
      1 -> Set(2, 3),
      2 -> Set(1, 3),
      3 -> Set(1, 2, 4),
      4 -> Set(1)
    )) shouldBe true
  }

  it should "find the maximum number of edges that can be removed to create an even forest" in {
    evenForest(Map(
      1 -> Set(2, 3),
      3 -> Set(4, 5),
      4 -> Set(6, 7, 8)
    ), 1) shouldBe 2
  }

  it should "determine if it's possible to finish all courses" in {
    courseSchedule(2, Map(1 -> Set(0))) shouldBe true
    courseSchedule(2, Map(1 -> Set(0), 0 -> Set(1))) shouldBe false
  }

  it should "find the largest value path of the graph" in {
    largestValuePath("ABACA", Seq((0, 1), (0, 2), (2, 3), (3, 4))) shouldBe "AACA"
    largestValuePath("A", Seq((0, 0))) shouldBe empty
  }

  it should "construct a De Bruijn Sequence for a given C and k" in {
    val seq = deBruijnSeq(2, 3)
    seq.length shouldBe 8
    val r1 = seq.toCharArray.map(_.toInt)
    val r2 = seq.toCharArray.map(_.toInt)
    rotateRight(r1, 1)
    rotateRight(r2, 2)
    val rotations = Seq(r1.map(_.toChar).mkString, r2.map(_.toChar).mkString, seq)

    crossPdt((0 to 1).map(_.toString), 3)
      .foreach(x => {
        rotations.exists(findAllOccurrences(_, x).size == 1) shouldBe true
      } withClue s", pattern: $x, sequence: $seq")
  }

  it should "compute the cheapest itinerary with at most k hops" in {
    val (route, price) = cheapestItinerary(Seq(
      ("JFK", "ATL", 150),
      ("ATL", "SFO", 400),
      ("ORD", "LAX", 200),
      ("LAX", "DFW", 80),
      ("JFK", "HKG", 800),
      ("ATL", "ORD", 90),
      ("JFK", "LAX", 500)
    ), 3, "JFK", "LAX")
    route should contain theSameElementsInOrderAs Seq("JFK", "ATL", "ORD", "LAX")
    price shouldBe 440
  }

  it should "count the number of islands" in {
    numIslands(IndexedSeq(
      IndexedSeq(1, 1, 1, 1, 0),
      IndexedSeq(1, 1, 0, 1, 0),
      IndexedSeq(1, 1, 0, 0, 0),
      IndexedSeq(0, 0, 0, 0, 0)
    )) shouldBe 1
    numIslands(IndexedSeq(
      IndexedSeq(1, 1, 0, 0, 0),
      IndexedSeq(1, 1, 0, 0, 0),
      IndexedSeq(0, 0, 1, 0, 0),
      IndexedSeq(0, 0, 0, 1, 1)
    )) shouldBe 3
  }

  it should "check if the sum of the rules validate" in {
    areValidRules(Seq(
      ('A', "N", 'B'),
      ('B', "NE", 'C'),
      ('C', "N", 'A')
    )) shouldBe false

    areValidRules(Seq(
      ('A', "NW", 'B'),
      ('A', "N", 'B')
    )) shouldBe true
  }

  it should "minimum number of steps from source to destination" in {
    minSteps(Seq((0, 0), (1, 1), (1, 2))) shouldBe 2
  }
}

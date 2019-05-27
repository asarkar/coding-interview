package org.asarkar.codinginterview.graphs

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class TopSortSpec extends FlatSpec {
  "TopSort" should "sort the vertices in topological order" in {
    val topSort = new TopSort[Char](
      collection.Map(
        'A' -> collection.Set('C'),
        'B' -> collection.Set('C', 'D'),
        'C' -> collection.Set('E'),
        'D' -> collection.Set('F'),
        'E' -> collection.Set('H', 'F'),
        'F' -> collection.Set('G')
      ),
      'A' to 'H'
    )
      .stack.mkString

    // see topsort.png
    Seq("BDACEFGH", "BDACEHFG") should contain(topSort)
  }

  it should "sort courses honoring prerequisites" in {
    new TopSort[String](
      collection.Map(
        "CSC300" -> collection.Set("CSC100", "CSC200"),
        "CSC200" -> collection.Set("CSC100")
      ),
      Seq("CSC100", "CSC200", "CSC300")
    )
      .stack.reverse should contain theSameElementsInOrderAs Seq("CSC100", "CSC200", "CSC300")
  }
}

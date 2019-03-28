package org.asarkar.codinginterview

import scala.collection.mutable

package object graphs {

  /*
   * Suppose you are given a table of currency exchange rates, represented as a 2D array. Determine whether there is a
   * possible arbitrage: that is, whether there is some sequence of trades you can make, starting with some amount A of
   * any currency, so that you can end up with some amount greater than A of that currency.
   * There are no transaction costs and you can trade fractional quantities.
   *
   * ANSWER: Suppose, 1 U.S. dollar bought 0.82 Euro, 1 Euro bought 129.7 Japanese Yen, 1 Japanese Yen bought 12
   * Turkish Lira, and 1 Turkish Lira bought 0.0008 U.S. dollars.
   * Then, by converting currencies, a trader can start with 1 U.S. dollar and buy 0.82 x 129.7 x 12 x 0.0008
   * ≅ 1.02 U.S dollars, thus doing arbitrage.
   *
   * To solve this problem, we will convert it to a graph problem, where every currency is a vertex, and the exchange
   * rates are directed edges. Note that it is going to be a complete graph (each pair of vertices is connected by an
   * edge).
   * The problem then becomes finding a cycle in a directed graph where the product of the edge weights is greater than
   * one.
   *
   * We know that log(ab) = log(a) + log(b) > 0. Taking the negative of both sides, -log(a) - log(b) < 0. Thus, if
   * we represent the edge weights are the negative logarithms of the exchange rates, the problem becomes finding a
   * negative cost cycle in a directed graph. Enter the Bellman-Ford algorithm!
   *
   * Since we are only interested in whether or not a negative cycle exists, not the actual shortest path (if any),
   * we don't need to store all values of i, just two (the current and the previous rows).
   *
   * See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/honors/package.scala
   */

  /*
   * Given an unordered list of flights taken by someone, each represented as (origin, destination) pairs, and a
   * starting airport, compute the person's itinerary. If no such itinerary exists, return null. If there are multiple
   * possible itineraries, return the lexicographically smallest one. All flights must be used in the itinerary.
   *
   * For example, given the list of flights [('SFO', 'HKO'), ('YYZ', 'SFO'), ('YUL', 'YYZ'), ('HKO', 'ORD')] and
   * starting airport 'YUL', you should return the list ['YUL', 'YYZ', 'SFO', 'HKO', 'ORD'].
   *
   * Given the list of flights [('SFO', 'COM'), ('COM', 'YYZ')] and starting airport 'COM', you should return null.
   *
   * Given the list of flights [('A', 'B'), ('A', 'C'), ('B', 'C'), ('C', 'A')] and starting airport 'A', you should
   * return the list ['A', 'B', 'C', 'A', 'C'] even though ['A', 'C', 'A', 'B', 'C'] is also a valid itinerary.
   * However, the first one is lexicographically smaller.
   *
   * ANSWER: The problem is basically asking to find an Eulerian path in a directed graph, starting from the given
   * vertex. We represent the graph as an adjacency list that we build from the given flights.
   *
   * For the existence of Eulerian paths, it is necessary that zero or two vertices have an odd degree, and the rest
   * have even degree. It intuitively makes sense; the start vertex should have indegree 0 and outdegree 1, and the
   * end vertex should have indegree 1 and outdegree 0. All other vertices should have indegree = outdegree, so that
   * it's not possible to get stuck at a vertex. If there are no vertices of odd degree, all Eulerian paths are
   * circuits, which for this problem, is not applicable.
   *
   * We check if the start node satisfies the condition, outdegree - indegree = 1, and if not, don't even bother to
   * search. We can also verify if an Eulerian path exists for the given graph, but since the search takes O(|E|) time
   * anyway, we just do the search instead and see what it tells us.
   *
   * The search is simply DFS from the starting node.
   * We account for the fact that the end vertex is not in the flight map, since there are no outgoing edges from it.
   *
   * In the end, we simply check if the path we found has the same number of edges as the input.
   *
   * Couple of good, but somewhat slow, YouTube videos.
   * Existence of Eulerian Paths and Circuits https://www.youtube.com/watch?v=xR4sGgwtR2I
   * Eulerian Path Algorithm https://www.youtube.com/watch?v=8MpoO2zA2l4
   */
  def computeItinerary(flights: Seq[(String, String)], start: String): Seq[String] = {
    var indegree = 0
    val flightMap = flights
      .foldLeft(mutable.Map.empty[String, mutable.SortedSet[String]]) { (acc, flight) =>
        val (from, to) = flight
        if (to == start) indegree += 1
        acc.updated(from, acc.getOrElse(from, mutable.SortedSet.empty[String]) += to)
      }

    def dfs(from: String): Seq[String] = {
      flightMap.get(from) match {
        case Some(to) =>
          to.headOption match {
            case Some(first) =>
              assert(to.remove(first))
              if (to.isEmpty) flightMap.remove(from)

              first +: dfs(first)
            case _ => Seq.empty[String]
          }
        case _ => Seq.empty[String]
      }
    }

    val outdegree = flightMap.get(start).map(_.size).getOrElse(0)

    if (outdegree - indegree != 1) Seq.empty[String]
    else dfs(start) match {
      case xs if xs.size == flights.size => start +: xs
      case _ => Seq.empty[String]
    }
  }
}

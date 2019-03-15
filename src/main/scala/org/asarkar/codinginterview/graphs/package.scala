package org.asarkar.codinginterview

import scala.annotation.tailrec

package object graphs {

  def isArbitragePossible(rates: IndexedSeq[IndexedSeq[Double]]): Boolean = {
    val n = rates.size
    // dp[i][j] is the distance between the source and vertex j using no more than i edges
    // when i = 0, we can only go from the source to the source using the empty edge (length zero)
    val dp = Array.tabulate[Double](2, n)((i, v) => if (i == 0 && v == 0) 0d else Double.PositiveInfinity)

    val negLogRates = rates
      .map(_.map { r =>
        assert(r.notApproxEquals(0.0d), "Exchange rate must be a positive decimal")
        -math.log(r)
      })

    @tailrec
    def hasNegCycle(i: Int, v: Int): Boolean = {
      if (i <= n) {
        val cur = i % dp.length
        val prev = cur ^ 1
        dp(cur)(v) = math.min(
          dp(prev)(v),
          (0 until n)
            .filterNot(_ == v)
            .map(w => dp(prev)(w) + negLogRates(w)(v))
            .min
        )
        if (i == n && dp(cur)(v).notApproxEquals(dp(prev)(v))) true
        else if (v < n - 1) hasNegCycle(i, v + 1)
        else hasNegCycle(i + 1, 0)
      } else false
    }

    hasNegCycle(1, 0)
  }

  implicit val precision: Precision = Precision(0.01d)

  implicit class DoubleOps(val d: Double) extends AnyVal {
    def notApproxEquals(other: Double)(implicit p: Precision): Boolean = (d - other).abs >= p.p
  }

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
   * Since this is a dense graph, time complexity is O(n^3). Space complexity is O(n).
   */
  case class Precision(p: Double)
}

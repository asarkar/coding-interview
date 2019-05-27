package org.asarkar.codinginterview

import java.awt.geom.Point2D

import scala.annotation.tailrec
import scala.collection.generic.FilterMonadic
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
   * search. We can also verify if an Eulerian path exists for the given graph by checking the degrees of all the
   * vertices, but since the search takes O(|E|) time anyway, we just do the search instead and see what it tells us.
   * In the end, we check if all the edges were visited.
   *
   * The search is simply DFS from the starting node. We recursively visit all unvisited edges, and when there are
   * none, prepend the current node to the itinerary. This is the Hierholzer's algorithm.
   * (https://en.wikipedia.org/wiki/Eulerian_path#Hierholzer's_algorithm).
   *
   * Time complexity: O(\E\), since we visit each edge only once.
   *
   * Couple of good, but somewhat slow, YouTube videos.
   * Existence of Eulerian Paths and Circuits https://www.youtube.com/watch?v=xR4sGgwtR2I
   * Eulerian Path Algorithm https://www.youtube.com/watch?v=8MpoO2zA2l4
   */
  def computeItinerary(flights: Seq[(String, String)], start: String): Seq[String] = {
    var indegree = 0
    val flightMap = flights
      .foldLeft(mutable.Map.empty[String, mutable.PriorityQueue[String]]) { (acc, flight) =>
        val (from, to) = flight
        if (to == start) indegree += 1
        acc += from -> (acc.getOrElse(from, mutable.PriorityQueue.empty[String](Ordering[String].reverse)) += to)
      }

    def dfs(from: String, itinerary: ListBuffer[String]): Seq[String] = {
      flightMap.get(from) match {
        case Some(dest) if dest.nonEmpty =>
          dfs(dest.dequeue(), itinerary)
          dfs(from, itinerary)
        case _ =>
          flightMap.remove(from)
          from +=: itinerary
      }
    }

    val outdegree = flightMap.get(start).map(_.size).getOrElse(0)

    if (outdegree - indegree != 1) Seq.empty[String]
    else {
      val itinerary = dfs(start, ListBuffer.empty[String])
      if (flightMap.isEmpty) itinerary
      else Seq.empty[String]
    }
  }

  /*
   * Check if a undirected graph has a cycle.
   */
  def hasCycle(graph: Map[Int, Set[Int]]): Boolean = {
    new TopSort(graph, Seq(graph.head._1)).hasCycle
  }

  /*
   * Given an undirected tree which has even number of vertices, we need to remove the maximum number of edges from
   * this tree such that each connected component of the resultant forest has an even number of vertices.
   *
   * Example:
   *     1
   *     +
   * 2<--+-->3
   *     +---+---+
   *     4^---+--^5
   * ----^----
   * 6   7   8
   *
   * We can remove the edges 1 -> 3 and 3 -> 4 to create the even forests {4, 6, 7, 8} and {3, 4, 5, 6, 7, 8}.
   *
   * ANSWER: Observe that the edge between any node and its parent can be removed if the node has an odd number
   * of descendants. We first compute the size of the subtrees at each node by running DFS. The count of all nodes
   * that have odd number of descendants is the answer (except for the root itself, since it has no parent).
   */
  def evenForest(graph: Map[Int, Set[Int]], root: Int): Int = {
    val children = mutable.Map.empty[Int, Int]

    def loop(v: Int): Int = {
      val adj = graph.getOrElse(v, Set.empty[Int])
      children(v) = adj.size + adj
        .map(loop)
        .sum

      children(v)
    }

    loop(root)

    children.count(x => x._2 % 2 != 0 && x._1 != root)
  }

  /*
   * There are a total of n courses you have to take, labeled from 0 to n-1.
   * Some courses may have prerequisites, for example to take course 0 you have to first take course 1, which is
   * expressed as a tuple: 0 -> 1.
   *
   * Given the total number of courses and a list of prerequisite pairs, is it possible for you to finish all courses?
   *
   * Example 1:
   * Input: 2, [1 -> 0]
   * Output: true
   * Explanation: There are a total of 2 courses to take. To take course 1 you should have finished course 0.
   * So it is possible.
   *
   * Example 2:
   * Input: 2, [1 -> 0, 0 -> 1]
   * Output: false
   * Explanation: There are a total of 2 courses to take. To take course 1 you should have finished course 0, and to
   * take course 0 you should also have finished course 1. So it is impossible.
   *
   * ANSWER: Run DFS; if at any time, we find a back edge, it's not possible to finish the courses.
   */
  def courseSchedule(numCourses: Int, prerequisites: Map[Int, Set[Int]]): Boolean = {
    !new TopSort(prerequisites, 0 until numCourses).hasCycle
  }

  /*
   * In a directed graph, each node is assigned an uppercase letter. We define a path's value as the number of most
   * frequently-occurring letter along that path. For example, if a path in the graph goes through "ABACA", the value
   * of the path is 3, since there are 3 occurrences of 'A' on the path.
   *
   * Given a graph with n nodes and m directed edges, return the largest value path of the graph. If the largest value
   * is infinite, then return null.
   *
   * The graph is represented with a string and an edge list. The i-th character represents the uppercase letter of
   * the i-th node. Each tuple in the edge list (i, j) means there is a directed edge from the i-th node to the j-th
   * node. Self-edges are possible, as well as multi-edges.
   *
   * For example, the following input graph:
   * ABACA
   * [(0, 1), (0, 2), (2, 3), (3, 4)]
   * Would have maximum value 3 using the path of vertices [0, 2, 3, 4], (A, A, C, A).
   *
   * The following input graph:
   * A
   * [(0, 0)]
   * Should return null, since we have an infinite loop.
   *
   * ANSWER: The problem can be thought of as finding a simple path of maximal length in a graph. In general, the
   * longest path problem does not have optimal substructure, and is NP-Hard. Consider the following undirected graph:
   *
   * B+----+A
   * +    + +
   * +  +   +
   * C+----+D
   * +    +
   * +  +
   * E
   *
   * The longest path from A to E is A-B-C-D-E. This goes through B, but the longest path from B to E is not B-C-D-E,
   * it’s B-C-A-D-E. This problem does not have optimal substructure, which means that we cannot solve it using
   * dynamic programming.
   *
   * Fortunately, the longest path in a DAG does have optimal substructure, which allows us to solve for it using
   * dynamic programming. For a graph G = (V, E), the algorithm is shown below.
   *
   * for each vertex v ∈ V in linearized order
   *   do dist(v) = max(u,v) ∈ E {dist(u) + 1}
   *
   * If there are no edges into a vertex (ie., if in-degree[v] = 0), we take the maximum over an empty set, so dist(v)
   * is 0 as expected.
   * And, we can compute this bottom-up for each vertex v ∈ V taken in a linearized order. The final algorithm is
   * simply taking the max of all dist(v).
   *
   * See https://blog.asarkar.org/assets/docs/algorithms-curated/Longest%20Path%20in%20a%20DAG%20-%20Khan.pdf
   *
   * Time complexity:
   *
   * Topological sort: O(V + E).
   * For each vertex, we iterate over all its neighbors. Overall, we only process each edge in the graph once, so
   * for a sparse graph, that takes O(V + E) time.
   * Finally, we backtrack from the node with the max distance, which in the worst case, may include all vertices in
   * the graph. Overall time complexity O(V + E) time.
   *
   * Space complexity: O(V + E), since we store every edge twice, once for outgoing, and once for incoming.
   */
  def largestValuePath(word: String, edges: Seq[(Int, Int)]): String = {
    val outgoing = new mutable.HashMap[Int, mutable.Set[Int]] with mutable.MultiMap[Int, Int]
    val incoming = new mutable.HashMap[Int, mutable.Set[Int]] with mutable.MultiMap[Int, Int]

    edges
      .foreach { case (w, v) =>
        outgoing.addBinding(w, v)
        incoming.addBinding(v, w)
      }

    val topSort = new TopSort(outgoing.mapValues(_.toSet), 0 until word.length)
    if (topSort.hasCycle) ""
    else {
      val dist = Array.fill[Double](word.length)(Double.NegativeInfinity)
      val prev = Array.ofDim[Int](word.length)

      val i = topSort.stack
        .foldLeft((0d, -1)) { case ((maxDist, maxDistV), v) =>
          // this works only because of the topological ordering
          val (d, p) = incoming.getOrElse(v, mutable.Set.empty[Int])
            .foldLeft((0d, -1)) { case ((max, neighbor), w) =>
              if (max > dist(w) + 1) (max, neighbor) else (dist(w) + 1, w)
            }

          dist(v) = d
          prev(v) = p

          if (maxDist > dist(v)) (maxDist, maxDistV) else (dist(v), v)
        }
        ._2

      Iterator.iterate(i)(prev)
        .takeWhile(word.isDefinedAt)
        .map(word)
        .foldLeft(ListBuffer.empty[Char])((acc, ch) => ch +=: acc)
        .mkString
    }
  }

  def crossPdt(alphabet: Seq[String], k: Int): Seq[String] = {
    Iterator.iterate(alphabet) { xs =>
      for (s <- xs; i <- alphabet) yield s"$s$i"
    }
      .dropWhile(_.headOption.map(_.length).getOrElse(k) < k)
      .take(1)
      .next()
  }

  /*
   * For a set of characters C and an integer k, a De Bruijn sequence is a cyclic sequence in which every possible
   * k-length string of characters in C occurs exactly once.
   * For example, suppose C = {0, 1} and k = 3. Then our sequence should contain the substrings {'000, '001', '010',
   * '011', '100', '101', '110', '111'}, and one possible solution would be 000101111.
   *
   * Create an algorithm that find a De Bruijn sequence for a given C and k.
   *
   * ANSWER: This can be formulated as a graph problem where the node labels are k - 1 length substrings drawn from
   * the cartesian product of the alphabet C where each element has k - 1 characters, and edge labels are single
   * characters such that label of the edge (u, v) is the last character of v. For example, for C = {0, 1} and k = 3,
   * the graph G would be G(V) = {00, 01, 11, 10} and G(E) = {(00, 00), (00, 01), (01, 10), (01, 11), (11, 11),
   * (11, 10), (10, 00), (10, 01)}. By traversing the edge (00, 01), we can form the substring 001.
   *
   * Observe that every node has k incoming and k outgoing vertices. That means there exists an Eulerian cycle for this
   * graph. We will use Hierholzer's algorithm to traverse all the edges.
   *
   * We run a DFS from an arbitrary node, and keep appending the nodes we visit to the path until we come back to the
   * starting node. We then find a node in the path that has yet unvisited neighbors, and start another DFS. When
   * the second BFS returns, we find the starting node of the second path in the first one, and replace it with the
   * second path. This is basically backtracking, but we maintain the continuity by replacing the node with a cycle
   * as if all the edges from that node are explored before moving on to another node.
   * For example, if the first path is 00 -> 00, and second 00 -> 01 -> 10 -> 00, the head of the second node is the
   * starting node, and its also the head of the first node. The  resulting path would be obtained by replacing the
   * head of the first node with the second path, giving us 00 -> 01 -> 10 -> 00 -> 00. We continue in this manner
   * until all the nodes have been visited.
   *
   * Since we found a cycle, we drop the last node and simply concatenate the last characters of the node labels in the
   * path. That gives us a De Bruijn sequence.
   *
   */
  def deBruijnSeq(n: Int, k: Int): String = {
    val alphabet = (0 until n).map(_.toString)

    val graph = crossPdt(alphabet, k - 1)
      .foldLeft(mutable.Map.empty[String, mutable.Buffer[String]]) { (g, v) =>
        g += (v -> mutable.Buffer(alphabet.map(w => s"${v.drop(1)}$w"): _*))
      }

    @tailrec
    def visit(start: String, v: String, path: mutable.Buffer[String]): mutable.Buffer[String] = {
      if (start == v) path += v
      else {
        val edges = graph(v)
        val w = edges.remove(0)

        if (edges.isEmpty) graph.remove(v)
        visit(if (start.isEmpty) v else start, w, path += v)
      }
    }

    Iterator.iterate(visit("", graph.head._1, mutable.Buffer.empty)) { path =>
      path
        .find(graph.contains)
        .toBuffer[String]
        .flatMap(start => visit("", start, mutable.Buffer.empty))
    }
      .takeWhile(_.nonEmpty)
      .reduce { (a, b) =>
        val insertionPoint = a.indexOf(b.head)
        val (l, r) = a.splitAt(insertionPoint)
        l ++= b ++= r.tail
      }
      .dropRight(1)
      .map(_.takeRight(1))
      .mkString
  }

  /*
   * You are given a huge list of airline ticket prices between different cities around the world on a given day.
   * These are all direct flights. Each element in the list has the format (source, destination, price).
   *
   * Consider a user who is willing to take up to k connections to get from their origin city A to their destination
   * city B. Find the cheapest fare possible for this journey and print the itinerary.
   *
   * For example, our traveler wants to go from JFK to LAX with up to 3 connections, and our input flights are as
   * follows:
   * [
   *   ("JFK", "ATL", 150),
   *   ("ATL", "SFO", 400),
   *   ("ORD", "LAX", 200),
   *   ("LAX", "DFW", 80),
   *   ("JFK", "HKG", 800),
   *   ("ATL", "ORD", 90),
   *   ("JFK", "LAX", 500)
   * ]
   *
   * Due to some improbably low flight prices, the cheapest itinerary would be JFK -> ATL -> ORD -> LAX, costing $440.
   *
   * ANSWER: This problem exhibits both properties required for dynamic programming:
   * - Optimal substructure: Cheapest price from A to C with at most k hops is given by cheapest price from A to
   *   an intermediate airport B with at most k - 1 hops, plus the flight cost from B to C.
   * - Overlapping subproblems: Clearly, cheapest flight between two different airports may involve similar routes for
   *   parts of the journey.
   *
   * Let dp[i][j] be the cheapest price to airport ifrom the source  with at most j hops. We simply iterate bottom up
   * using the optimal substructure property. In the end, the cheapest price to the destination would be in dp[dest][k].
   *
   * Time complexity: If the number of flights is E, and the number of airports is V, building the map of incoming
   * edges takes O(E) time. The fop loop runs for O(V * (k - 1)) or O(kV) times. Reconstructing the path may take O(V)
   * time. Overall time complexity is O(E + kV).
   *
   * Another way of solving this problem would be by Dijkstra's SP algorithm, using the number of hops left as a
   * secondary key for the priority queue nodes. First key, of course, would be the total cost to get to the current
   * node. At each iteration, we would pick the lowest cost flight from an airport, and relax the edges for all its
   * neighbors. We stop when we reach the destination.
   */
  def cheapestItinerary(flights: Seq[(String, String, Int)], k: Int, src: String, dest: String): (Seq[String], Int) = {
    val dp = mutable.Map.empty[(String, Int), Int]

    val incoming = flights
      .foldLeft(mutable.Map.empty[String, (String, Int)]) { case (x, (from, to, price)) =>
        if (x.contains(to)) x += to -> Seq(x(to), (from, price)).minBy(_._2)
        else x += to -> (from, price)

        if (from == src) dp += (to, 1) -> price

        x
      }

    for (v <- incoming.keys; i <- 2 to k) {
      val (u, p) = incoming(v)
      dp += (v, i) -> (dp.getOrElse((u, i - 1), 0) + p)
    }

    val it = Iterator.iterate((dest, k)) { case (airport, hops) =>
      val a = incoming(airport)._1
      (a, hops - 1)
    }
      .takeWhile(x => incoming.contains(x._1) && x._2 >= 1)
      .map(_._1)
      .foldLeft(ListBuffer.empty[String]) { (acc, x) =>
        x +=: acc
      }
    (src +=: it, dp(dest, k))
  }

  /*
   * Given a matrix of 1s and 0s, return the number of "islands" in the matrix. A 1 represents land and 0 represents
   * water, so an island is a group of 1s that are neighboring whose perimeter is surrounded by water.
   *
   * For example, this matrix has 4 islands.
   *
   * 1 0 0 0 0
   * 0 0 1 1 0
   * 0 1 1 0 0
   * 0 0 0 0 0
   * 1 1 0 0 1
   * 1 1 0 0 1
   *
   * ANSWER: This is a problem of finding connected components in an undirected graph. We can do that simply by
   * launching DFS/BFS from every vertex that's not been visited and is an one. The number of times we launch DFS
   * is the number of connected components, i.e. islands.
   *
   * Time complexity: O(mn).
   */
  def numIslands(grid: IndexedSeq[IndexedSeq[Int]]): Int = {
    val visited = mutable.Set.empty[(Int, Int)]

    def neighbors(row: Int, col: Int): FilterMonadic[(Int, Int), Seq[(Int, Int)]] = {
      Seq((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1))
        .withFilter { case (r, c) => grid.isDefinedAt(r) && grid(r).isDefinedAt(c) && grid(r)(c) == 1 &&
          !visited.contains((r, c))
        }
    }

    def dfs(row: Int, col: Int): Unit = {
      visited.add((row, col))

      neighbors(row, col)
        .foreach { case (r, c) => dfs(r, c) }
    }

    (
      for (row <- grid.indices; col <- grid(row).indices if grid(row)(col) == 1 && !visited.contains((row, col)))
        yield dfs(row, col)
      ).count(_ => true)
  }

  /*
   * A rule looks like this:
   * A NE B
   *
   * This means this means point A is located northeast of point B.
   *
   * A SW C
   * means that point A is southwest of C.
   *
   * Given a list of rules, check if the sum of the rules validate. For example:
   * A N B
   * B NE C
   * C N A
   * does not validate, since A cannot be both north and south of C.
   *
   * A NW B
   * A N B
   * is considered valid.
   *
   * ANSWER: We assume that directions can only be North-ish and South-ish (not East-ish or West-ish). We create a
   * directed graph such that for every rule, there's an edge from the Southern vertex to the Northern one. We then
   * launch DFS from each unvisited node of the graph. If at any point during a DFS we encounter a back edge, then
   * there's a cycle, and the ruleset is invalid.
   * Time complexity: O(V + E)
   */
  def areValidRules(rules: Seq[(Char, String, Char)]): Boolean = {
    val graph = new mutable.HashMap[Char, mutable.Set[Char]] with mutable.MultiMap[Char, Char]

    rules.foreach { case (u, dir, v) =>
      if (dir.head == 'N') graph.addBinding(v, u)
      else graph.addBinding(u, v)
    }
    val visiting = mutable.Set.empty[Char]
    val visited = mutable.Set.empty[Char]

    def visit(v: Char): Boolean = {
      val x = visiting.add(v) &&
        graph.getOrElse(v, mutable.Set.empty[Char])
          .view
          .filterNot(visited.contains)
          .forall(visit) &&
        visiting.remove(v)
      visited.add(v)
      x
    }

    graph.keys.forall(visit)
  }

  /*
   * You are in an infinite 2D grid where you can move in any of the 8 directions. You are given a sequence of points
   * and the order in which you need to cover the points. Give the minimum number of steps in which you can achieve it.
   * You start from the first point.
   *
   * Example:
   * Input: [(0, 0), (1, 1), (1, 2)]
   * Output: 2
   *
   * It takes 1 step to move from (0, 0) to (1, 1). It takes one more step to move from (1, 1) to (1, 2).
   *
   * ANSWER: We consider pairwise points, and starting from the source, we choose the next neighbor such that the
   * distance to the destination is minimized. We recurse until we reach the destination.
   *
   * The total number of steps is the sum of the steps from each pair.
   */
  def minSteps(points: Seq[(Int, Int)]): Int = {
    def bestNeighbor(src: (Int, Int), dest: (Int, Int)): (Int, Int) = {
      val (x, y) = src
      Seq(
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1),
        (x - 1, y - 1),
        (x + 1, y + 1),
        (x - 1, y + 1),
        (x + 1, y - 1)
      )
        .minBy(p => Point2D.distanceSq(p._1, p._2, dest._1, dest._2))
    }

    points
      .sliding(2, 1)
      .foldLeft(0) { (sum, xs) =>
        sum + Iterator.iterate(xs.head) { x =>
          val best = bestNeighbor(x, xs.last)
          println(s"Best neighbor of $x is $best")
          best
        }
          .takeWhile(_ != xs.last) // doesn't include the last hop to the dest, but makes up by including the src
          .size
      }
  }
}

package org.asarkar.codinginterview

import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

package object recursion {
  /*
   * How many ways to decode this message? For example, given encoding a = 1, b = 2, c = 3, ..., z = 26, what does the
   * encoded string '12' represent? It could be 'ab' or 'l'. For simplicity, assume that the encoded string contains
   * only digits 0-9. Can you solve this in O(n) time, where n is the length of the encoded string?
   *
   * ANSWER: Observe that the number of ways is simply the number of leaves in the recursion tree. If we represent
   * the number of ways to decode a message of length n to be N(n), observe that N(n) = N(n - 1) + N(n - 2),
   * where the second term is present only if the first two characters converted to a digit is smaller than the maximum
   * digit in the encoding. For the given question, the maximum digit is 26 corresponding to the letter 'z'. Similarly,
   * if the message starts with a digit smaller than the minimum digit in the encoding (1, corresponding to 'a'), there
   * is no way to decode it.
   *
   * To avoid creating new strings, we introduce 'i' as the number of characters to consider from the right. Since the
   * subproblems are overlapping (notice the recurrence above, it looks very much like Fibonacci), we also memoize the
   * intermediate results to avoid recalculations.
   */
  def numWaysToDecode(msg: String): Int = {
    val dp = Array.fill[Int](msg.length + 1)(-1)
    val (lowest, highest) = (1, 26)

    def loop(i: Int): Int = {
      if (i == 0) 1
      else {
        val j = msg.length - i

        if (msg(j).asDigit < lowest) 0
        else if (dp(i) >= 0) dp(i)
        else {
          dp(i) = loop(i - 1) +
            (if (msg.isDefinedAt(j + 1) && msg.substring(j, j + 2).toInt <= highest) loop(i - 2)
            else 0)

          dp(i)
        }
      }
    }

    loop(msg.length)
  }

  //  val hamming: Stream[BigInt] = {
  //    // #::[B >: A](hd: B): Stream[B]
  //    // Construct a stream consisting of a given first element followed by elements from a lazily evaluated Stream.
  //    def merge(inx: Stream[BigInt], iny: Stream[BigInt]): Stream[BigInt] = {
  //      if (inx.head < iny.head) inx.head #:: merge(inx.tail, iny)
  //      else if (iny.head < inx.head) iny.head #:: merge(inx, iny.tail)
  //      else merge(inx, iny.tail)
  //    }
  //
  //    1 #:: merge(hamming.map(_ * 2), merge(hamming.map(_ * 3), hamming.map(_ * 5)))
  //  }

  object FileType extends Enumeration {
    type FileType = Value
    val File, Dir = Value
  }

  import FileType._

  case class Path(typ: FileType, name: String, parent: Path, level: Int)

  /*
   * Suppose we represent our file system by a string in the following manner:
   *
   * The string "dir\n\tsubdir1\n\tsubdir2\n\t\tfile.ext" represents:
   * dir
   *  subdir1
   *  subdir2
   *    file.ext
   *
   * The directory dir contains an empty sub-directory subdir1 and a sub-directory subdir2 containing a file file.ext.
   *
   * The string "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext" represents:
   * dir
   *   subdir1
   *     file1.ext
   *     subsubdir1
   *   subdir2
   *     subsubdir2
   *       file2.ext
   *
   * The directory dir contains two sub-directories subdir1 and subdir2. subdir1 contains a file file1.ext and an empty
   * second-level sub-directory subsubdir1. subdir2 contains a second-level sub-directory subsubdir2 containing a file
   * file2.ext.
   *
   * We are interested in finding the longest (number of characters) absolute path to a file within our file system.
   * For example, in the second example above, the longest absolute path is "dir/subdir2/subsubdir2/file2.ext", and its
   * length is 32 (not including the double quotes).
   *
   * Given a string representing the file system in the above format, return the length of the longest absolute path to
   * a file in the abstracted file system. If there is no file in the system, return 0.
   *
   * Note:
   * The name of a file contains at least a period and an extension.
   * The name of a directory or sub-directory will not contain a period.
   *
   * ANSWER: Observe that the file system lists all the subdirectories and files under a directory before listing its
   * siblings or higher up directories. That means if we process one segment of the file system at a time, indicating
   * the level of a segment by the number of preceding tabs, when we encounter a segment whose level is equal to or
   * smaller than the path we are exploring, we can be sure that we have completely explored the current path.
   *
   * This suggests a DFS as follows.
   * For the current segment:
   *  - If its level is greater than that of the current path, we continue exploring this path. Keeping with the DFS
   *    analogy, this is equivalent to recursively visiting a subtree of a node.
   *  - If its level is less than or equal to that of the current path, we climb up until we find a segment that falls
   *    in the above case. This is equivalent to finding a subtree higher up that has not yet been visited.
   *
   * The only other thing we need to do is to remember the longest path seen for a file during the traversal.
   *
   * We use regular expression for extracting the current segment. It works nicely because we can start a new match
   * from after the end of the previous match without having to extract substrings.
   */
  def longestFilePathLength(fileSys: String): Int = {
    val matcher = Pattern.compile("(?<tabs>\t*)(?<segment>.+)").matcher(fileSys)

    @tailrec
    def longestFilePath(path: Path, start: Int, longest: String): String = {
      if (start >= fileSys.length || !matcher.find(start)) longest
      else {
        val numTabs = matcher.group("tabs").length
        val segment = matcher.group("segment")
        val segmentTyp = if (segment.contains('.')) File else Dir

        if (numTabs > path.level) {
          val cur = Path(segmentTyp, s"${path.name}/$segment", path, numTabs)

          if (cur.typ == File)
            longestFilePath(path, matcher.end, if (cur.name.length > longest.length) cur.name else longest)
          else
            longestFilePath(cur, matcher.end, longest)
        } else longestFilePath(path.parent, start, longest)
      }
    }

    val longest = longestFilePath(Path(Dir, "", null, -1), 0, "")
      .drop(1) // drop starting /
    println(
      s"""The longest path to a file in the file system:
         |${fileSys.replace("\t", "\\t").replace("\n", "\\n")} is:
         |$longest""".stripMargin.replaceAll("\\R", " ")
    )
    longest.length
  }

  /*
   * Given a dictionary of words and a string made up of those words (no spaces), return the original sentence in a
   * list. If there is more than one possible reconstruction, return any of them. If there is no possible
   * reconstruction, then return null.
   *
   * For example, given the set of words 'quick', 'brown', 'the', 'fox', and the string "thequickbrownfox", you should
   * return ['the', 'quick', 'brown', 'fox'].
   * Given the set of words 'bed', 'bath', 'bedbath', 'and', 'beyond', and the string "bedbathandbeyond", return
   * either ['bed', 'bath', 'and', 'beyond] or ['bedbath', 'and', 'beyond'].
   *
   * ANSWER: We use a recursive backtracking approach with memoization. For each string, if it is not in the
   * dictionary, we remember it, and then try again by progressively splitting from the end. Each of these splits
   * correspond to a subtree in the recursion tree.
   * For example, consider the dictionary ["a", "b", "c", "ab", "bc", "abc"] and string "abcd". The corresponding
   * recursion tree is shown below. The dictionary checks are not show for brevity.
   * - Since "abcd" is not in the dictionary, we split it into "abc" and "d", and go into recursion with "d".
   * - Since "d" is not in the dictionary, we split into "ab" and "cd" and go into recursion with "cd", which, in turn,
   * recurses into "d". Now, having previously unsuccessfully checked "d", we remember it, and prune this branch
   * straightaway.
   * - Since "cd" is not in the dictionary, we split into "a" and "bcd", and go into recursion with "bcd", which,
   * in turn, recurses into "cd" and "d". Having previously unsuccessfully checked both, we prune these branches
   * without the need for further recursion.
   *
   *              abcd
   *   +-----------------------+
   *   bcd         cd         d
   *    +           +
   * +--+--+        +
   * cd    d        d
   *
   * A total of seven calls are made. Since length("abcd") = 4, the number of calls is O(2^(n-1)) - 1, so, the time
   * complexity is exponential.
   *
   * This problem can also be solve by Dynamic Programming, see https://www.youtube.com/watch?v=WepWFGxiwRs
   */
  def wordBreak(str: String, dict: Set[String]): String = {
    val nonDictWords = mutable.Set.empty[String]
    val statement = ListBuffer.empty[String]

    def isInDict(word: String) = {
      val inDict = dict.contains(word)
      if (inDict) println(s"'$word' is in the dictionary")
      inDict
    }

    def isValidWord(start: Int, end: Int): Boolean = {
      val substr = str.substring(start, end)
      println(s"Analyzing '$substr'")

      if (nonDictWords.contains(substr)) false
      else if (substr.isEmpty) true
      else {
        (substr.length to 1 by -1)
          .find(i => isInDict(substr.take(i)) && isValidWord(start + i, end)) match {
          case Some(i) =>
            statement.prepend(substr.take(i))
            true
          case _ =>
            println(s"'$substr' cannot be split into dictionary words")
            nonDictWords.add(substr)
            false
        }
      }
    }

    if (isValidWord(0, str.length)) statement.mkString(" ") else ""
  }

  /*
   * You are given an M by N matrix consisting of booleans that represents a board. Each True boolean represents a wall.
   * Each False boolean represents a tile you can walk on.
   * Given this matrix, a start coordinate, and an end coordinate, return the minimum number of steps required to reach
   * the end coordinate from the start. If there is no possible path, then return null. You can move up, left, down,
   * and right. You cannot move through walls. You cannot wrap around the edges of the board.
   *
   * For example, given the following board:
   * [[f, f, f, f],
   * [t, t, f, t],
   * [f, f, f, f],
   * [f, f, f, f]]
   * and start = (3, 0) (bottom left) and end = (0, 0) (top left), the minimum number of steps required to reach the
   * end is 7, since we would need to go through (1, 2) because there is a wall everywhere else on the second row.
   *
   * ANSWER: We will run BFS, without creating a graph explicitly. Given a M x N matrix, for each row, there are N - 1
   * horizontal edges, and since there are M rows, there are total M(N - 1) horizontal edges. Similarly, there are
   * N(M - 1) vertical edges. Total number of edges = M(N - 1) + N(M - 1) = 2MN - M - N. If M = N, this is a quadratic
   * number, and hence the graph is a dense graph.
   * Time complexity of BFS is O(|V| + |E|), and since |E| ≅ |V|x|V|, time complexity is O(|E|) or O(MN).
   */
  def minSteps(board: IndexedSeq[IndexedSeq[Boolean]], start: (Int, Int), end: (Int, Int)): Int = {
    val m = board.size
    val n = board.head.size
    val visited = Array.ofDim[Boolean](m, n)
    val queue = mutable.ListBuffer.empty[((Int, Int), Int)]

    def neighbors(row: Int, col: Int): Seq[(Int, Int)] = {
      Seq(
        (row - 1, col),
        (row, col - 1),
        (row + 1, col),
        (row, col + 1)
      )
        .filter { case (r, c) => board.isDefinedAt(r) && board(r).isDefinedAt(c) && !board(r)(c) }
    }

    @tailrec
    def search(): Int = {
      if (queue.isEmpty) -1
      else {
        val ((row, col), steps) = queue.remove(0)

        if ((row, col) == end) steps
        else {
          if (!visited(row)(col)) {
            println(s"Visiting: [$row][$col]")
            visited(row)(col) = true

            neighbors(row, col)
              // evaluate lazily, since the cell may be visited from another of its neighbor before it's visited
              // from this one
              .withFilter { case (r, c) => !visited(r)(c) }
              .foreach(x => queue.append((x, steps + 1)))
          }

          search()
        }
      }
    }

    queue.append((start, 0))
    search()
  }

  /*
   * You have an N by N board. Write a function that, given N, returns the number of possible arrangements of the board
   * where N queens can be placed on the board without threatening each other, i.e. no two queens share the same row,
   * column, or diagonal.
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/recursion/package.scala
   */

  /*
   * Write a method to return all valid combinations of n-pairs of parentheses.
   * The method should return an ArrayList of strings, in which each string represents a valid combination of
   * parentheses.
   * The order of the strings in the ArrayList does not matter.
   *
   * Examples: combParenthesis(2) ==> {"(())","()()"}
   * Note: Valid combination means that parentheses pairs are not left open. ")()(" is not a valid combination.
   */
  def combineParenthesis(n: Int): Seq[String] = {
    def loop(str: String, open: Int, close: Int): Seq[String] = {
      if (math.max(open, close) > n || close > open) Seq.empty[String]
      else if (close == n) Seq(str).filter(_.nonEmpty)
      else loop(str + ")", open, close + 1) ++ loop(str + "(", open + 1, close)
    }

    loop("", 0, 0)
  }

  /*
   * Given an undirected graph represented as an adjacency matrix and an integer k, write a function to determine
   * whether each vertex in the graph can be colored such that no two adjacent vertices share the same color using at
   * most k colors.
   *
   * ANSWER: Vertex coloring is in general a NP-hard problem. For small |V|, we may use backtracking to find a solution.
   * Without any constraints, each vertex can be colored in k ways, and there are n vertices, so the number of possible
   * combinations is O(k^n). Even with the constraint that no two adjacent vertices may share the same color, in the
   * worst case, we may backtrack k times for each vertex, and if the graph is complete, for each of the k iterations,
   * we try n - 1 neighbors. Thus time complexity is still O(k^n).
   *
   * The algorithm is as follows:
   *
   * for v in vertices
   *   if it's safe to assign color c
   *     assign color c to v, and recursively color the rest of the vertices
   *     if successful, return true
   *     else uncolor v and try the next color
   *   else try the next color
   * if all vertices have been colored, return true
   *
   * For k = 2, the problem becomes determining whether the given graph is bipartite or not.  Such a coloring of the
   * vertices of a bipartite graph means that the graph can be drawn with the red vertices on the left and the blue
   * vertices on the right such that all edges go from left to right.
   *
   * Testing whether a graph is bipartite is easy. Color the first vertex blue, and then do a depth-first search of the
   * graph. Whenever we discover a new, uncolored vertex, color it opposite that of its parent, since the same color
   * would cause a clash. If we ever find an edge where both vertices have been colored identically, then the graph
   * cannot be bipartite. Otherwise, this coloring will be a 2-coloring, and it is constructed in O(n + m) time.
   *
   * Application of vertex coloring:
   * Vertex coloring models to a number of scheduling problems. Given a set of jobs need to be assigned to time slots,
   * each job requires one such slot. Jobs can be scheduled in any order, but pairs of jobs may be in conflict in the
   * sense that they may not be assigned to the same time slot, for example because they both rely on a shared resource.
   * The corresponding graph contains a vertex for every job and an edge for every conflicting pair of jobs.
   * The chromatic number of the graph is exactly the minimum makespan, the optimal time to finish all jobs without
   * conflicts.
   *
   * Other examples can be found in this YouTube video: https://www.youtube.com/watch?v=y4RAYQjKb5Y
   */
  def isColorable(g: IndexedSeq[IndexedSeq[Boolean]], k: Int): Boolean = {
    val uncolored = -1
    val n = g.size
    val colors = Array.fill[Int](n)(uncolored)

    def isUnsafe(v: Int, c: Int): Boolean = {
      (0 until v)
        .exists(w => g(w)(v) && colors(w) == c)
    }

    def color(v: Int, c: Int): Boolean = {
      if (v >= n) true
      else if (isUnsafe(v, c)) color(v, c + 1)
      else {
        colors(v) = c
        if (color(v + 1, 0)) true
        else {
          colors(v) = uncolored
          color(v, c + 1)
        }
      }
    }

    color(0, 0)
  }

  /*
   * Implement integer exponentiation. That is, implement the pow(x, y) function, where x and y are integers and
   * returns x^y.
   * Do this faster than the naive method of repeated multiplication.
   *
   * For example, pow(2, 10) should return 1024.
   *
   * ANSWER: The recurrence relation is as follows:
   * x^y = x^(y/2) * x^(y/2) if y is even and positive -- case 1
   *     = x * x^(y-1) if y is odd and positive -- case 2
   *     = 1 if y = 0 -- base case
   *
   * If y is negative, we handle that in case 2; since case 1 simply multiplies the result with itself, it doesn't need
   * to worry about the sign of y. Also, since every value of y would eventually calculate y = 1, which is odd, we are
   * covered.
   * For example, fastPow(2, -3) -> fastPow(2, 2) -> fastPow(2, 1) -> fastPow(2, 0).
   * fastPow(2, 1) returns 1/2, fastPow(2, 2) returns 1/4, fastPow(2, -3) returns 1/8.
   *
   * Time complexity: Outside the recursive calls, the cost is only for multiplication. Since the input is halved each
   * time, the time complexity is O(log n) (there's one additional call for odd y that can safely be ignored for
   * Big-Oh).
   */
  def fastPow(x: Int, y: Int): Double = {
    if (y == 0) 1d
    else {
      if (y % 2 == 0) {
        val a = fastPow(x, y / 2)
        a * a
      } else {
        val a = fastPow(x, math.abs(y) - 1)
        if (y > 0) x * a else 1 / (x * a)
      }
    }
  }

  /*
   * Given a 2D board and a word, find if the word exists in the grid.
   * The word can be constructed from letters of sequentially adjacent cell, where "adjacent" cells are those
   * horizontally or vertically neighboring. The same letter cell may not be used more than once.
   *
   * ANSWER: Backtracking.
   */
  def hasWord(board: IndexedSeq[IndexedSeq[Char]], word: String): Boolean = {
    def neighbors(row: Int, col: Int): Seq[(Int, Int)] = {
      Seq((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1))
        .filter { case (r, c) => board.isDefinedAt(r) && board(r).isDefinedAt(c) }
    }

    def next(row: Int, col: Int, ch: Char): Option[(Int, Int)] = {
      if (board.isDefinedAt(row) && board(row).isDefinedAt(col)) {
        (row until board.size)
          .flatMap(r => ((if (r == row) col + 1 else 0) until board(r).size).map((r, _)))
          .find(x => board(x._1)(x._2) == ch)
      } else None
    }

    def exists(row: Int, col: Int, i: Int, visited: collection.mutable.Set[(Int, Int)]): Boolean = {
      if (i == word.length - 1 && board(row)(col) == word(i)) true
      else if (word.isDefinedAt(i) && board(row)(col) == word(i) && !visited.contains((row, col))) {
        visited += ((row, col))
        val found = neighbors(row, col)
          .view
          .filterNot(visited.contains)
          .exists { case (r, c) => exists(r, c, i + 1, visited) }
        if (!found) {
          // backtrack
          visited -= ((row, col))
        }
        found
      } else false
    }

    @tailrec
    def find(row: Int, col: Int): Boolean = {
      val visited = collection.mutable.Set.empty[(Int, Int)]
      if (exists(row, col, 0, visited)) true
      else next(row, col, word.head) match {
        case Some((r, c)) => find(r, c)
        case _ => false
      }
    }

    find(0, 0)
  }

  /*
   * A knight's tour is a sequence of moves by a knight on a chessboard such that all squares are visited once.
   * Given N, write a function to return the number of knight's tours on an N by N chessboard.
   *
   * ANSWER: We want to return a NxN matrix where the value of each cell is the i-th move; in other words, a value
   * of 5 would indicate the knight got to this cell in 4 moves from the initial cell (4 because the initial cell is
   * considered as the first move).
   * The usual approach to come mind is backtracking, where we visit each of the yet unvisited neighbors, and proceed
   * recursively. However, if the next neighbor is chosen arbitrarily, this can lead to a non-terminating solution.
   * From each cell, we have 8 potential neighbors to visit, and there are N^2 cells, so the time complexity is
   * O(8^(N^2))! This is not surprising, because the knight's tour can be reduced to finding a Hamiltonian path
   * problem, which is NP-Complete.
   *
   * A smarter way to choose the next neighbor is using a heuristic rule named Warnsdorff's rule, that chooses the
   * neighbor with the fewest possible unvisited neighbors (called degree in the code below). The code below breaks
   * ties arbitrarily, which has been shown to work for small boards (N < 40); for larger boards, there are smarter
   * ways of breaking ties. See https://blog.asarkar.org/coding-interview-curated/#recursion for some interesting
   * references.
   *
   * Using Warnsdorff's rule, this problem can be solved in O(N^2) time, using O(N^2) space for tracking the visited
   * cells.
   */
  def knightsTour(size: Int): IndexedSeq[IndexedSeq[Int]] = {
    val UNVISITED = -1
    val moves = Array.fill[Int](size, size)(UNVISITED)

    def neighbors(row: Int, col: Int): Seq[(Int, Int)] = {
      // see knight.png
      Seq(
        (row - 2, col - 1), (row - 2, col + 1),
        (row + 2, col - 1), (row + 2, col + 1),
        (row - 1, col - 2), (row - 1, col + 2),
        (row + 1, col - 2), (row + 1, col + 2)
      )
        .view
        .filter { case (r, c) => r >= 0 && r < size && c >= 0 && c < size && moves(r)(c) == UNVISITED }
    }

    def degree(row: Int, col: Int): Int = {
      neighbors(row, col)
        .size
    }

    def visit(row: Int, col: Int, count: Int): Boolean = {
      moves(row)(col) = count
      if (count == size * size) true
      else {
        neighbors(row, col) match {
          case Seq() => false
          case neighbors =>
            val best = neighbors.minBy(n => degree(n._1, n._2))
            visit(best._1, best._2, count + 1)
        }
      }
    }

    if (visit(0, 0, 1)) moves.map(_.toIndexedSeq).toIndexedSeq
    else IndexedSeq.empty
  }
}

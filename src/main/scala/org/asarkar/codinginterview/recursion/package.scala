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
   * ANSWER: We will run Dijkstra's SSSP algorithm, without creating a graph explicitly, and without using a min heap.
   * Since the number of neighbors for any cell can be at max four, sorting the list of neighbors takes O(1) time.
   */
  def minSteps(board: IndexedSeq[IndexedSeq[Boolean]], start: (Int, Int), end: (Int, Int)): Int = {
    val m = board.size
    val n = board.head.size
    val dist = Array.tabulate[Int](m, n)((r, c) => if ((r, c) == start) 0 else Int.MaxValue)
    val visited = Array.ofDim[Boolean](m, n)

    def neighbors(row: Int, col: Int): Seq[(Int, Int)] = {
      Seq(
        (row - 1, col),
        (row, col - 1),
        (row + 1, col),
        (row, col + 1)
      )
        .filter { case (r, c) => board.isDefinedAt(r) && board(r).isDefinedAt(c) && !board(r)(c) }
    }

    def visit(row: Int, col: Int): Unit = {
      println(s"Visiting: [$row][$col]")
      visited(row)(col) = true

      val xs = neighbors(row, col)
        // evaluate lazily, since the cell may be visited from another of its neighbor before it's visited from this
        // one
        .view
        .filterNot { case (r, c) => visited(r)(c) }

      xs
        // decrease key
        .foreach { case (r, c) => dist(r)(c) = math.min(dist(row)(col) + 1, dist(r)(c)) }

      xs
        .sortBy(x => dist(x._1)(x._2))
        .filterNot(_ => visited(end._1)(end._2))
        // extract min
        .foreach { case (r, c) => visit(r, c) }
    }

    visit(start._1, start._2)
    dist(end._1)(end._2)
  }
}

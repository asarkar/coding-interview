package org.asarkar.codinginterview

package object sorting {
  /*
   * 2-sum
   *
   * ANSWER: We make the following assumptions:
   * 1. Input array contains only distinct elements
   * 2. Order doesn't matter, i.e. (1, 3) = (3, 1)
   * 3. An element can't be paired with itself, i.e. (1, 1) is not a valid answer for k = 2.
   *
   * Time complexity: O(n log(n)), space complexity: O(n) (could be O(1) if we sort in-place)
   */
  def twoSum(xs: IndexedSeq[Int], k: Int): Seq[(Int, Int)] = {
    twoSum(xs.sorted, k, 0, xs.size - 1)
  }

  def twoSum(xs: IndexedSeq[Int], k: Int, start: Int, end: Int): Seq[(Int, Int)] = {
    Iterator.iterate((start, end, collection.mutable.Set.empty[(Int, Int)])) { case (i, j, acc) =>
      if (i < j && xs(i) + xs(j) == k) {
        val x = if (xs(i) < xs(j)) (xs(i), xs(j)) else (xs(j), xs(i))
        if (!acc.contains(x)) acc += x

        (i + 1, j - 1, acc)
      } else if (xs(i) + xs(j) < k) (i + 1, j, acc)
      else (i, j - 1, acc)
    }
      .dropWhile(x => x._1 < x._2)
      .take(1)
      .map(_._3.toSeq)
      .next()
  }

  /*
   * 3-sum
   *
   * ANSWER: Using 2-sum as a subroutine, for each element x, we check for pairs that sum to k - x.
   *
   * Time complexity: O(n²), space complexity: O(n)
   */
  def threeSum(xs: IndexedSeq[Int], k: Int): Seq[(Int, Int, Int)] = {
    threeSum(xs.sorted, k, 0, xs.size - 1)
  }

  def threeSum(xs: IndexedSeq[Int], k: Int, start: Int, end: Int): Seq[(Int, Int, Int)] = {
    (start to end)
      .flatMap(i => twoSum(xs, k - xs(i), i + 1, xs.size - 1)
        .map(x => Seq(xs(i), x._1, x._2).distinct.sorted)
        .filter(_.size == 3)
        .map(zs => (zs.head, zs.tail.head, zs.last))
      )
  }

  /*
   * 4-sum
   *
   * ANSWER: Using 3-sum as a subroutine, for each element x, we check for triplets that sum to k - x.
   *
   * Time complexity: O(n³), space complexity: O(n)
   */
  def fourSum(xs: IndexedSeq[Int], k: Int): Seq[(Int, Int, Int, Int)] = {
    val ys = xs.sorted

    ys.indices
      .flatMap(i => threeSum(ys, k - ys(i), i + 1, ys.size - 1)
        .map(x => Seq(ys(i), x._1, x._2, x._3).distinct.sorted)
        .filter(_.size == 4)
        .map(zs => (zs.head, zs.tail.head, zs.tail.tail.head, zs.last))
      )
  }
}

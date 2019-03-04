package org.asarkar.codinginterview.lists

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

class ListsSpec extends FlatSpec {
  "lists" should "find the kth node in the given list" in {
    kthNode(LinkedList(1, 2, 3, 4), 2).value should be(2)
    kthNode(LinkedList(1, 2, 3, 4), 5) shouldBe empty
  }

  it should "find the kth node from the end in the given list" in {
    kthNodeFromTheEnd(LinkedList(1, 2, 3, 4), 3).value should be(2)
    kthNodeFromTheEnd(LinkedList(1, 2, 3, 4), 5) shouldBe empty
  }

  it should "sort the given list" in {
    mergeSort(LinkedList(3, 2, 5, 1, 7, 5, 3)).toSeq shouldBe sorted
  }

  it should "return first and second element of a pair from car and cdr" in {
    car(cons(3, 4)) shouldBe 3
    cdr(cons(3, 4)) shouldBe 4
  }
}

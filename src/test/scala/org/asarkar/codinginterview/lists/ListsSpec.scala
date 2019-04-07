package org.asarkar.codinginterview.lists

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.JavaConverters._

class ListsSpec extends FlatSpec with TableDrivenPropertyChecks {
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

  it should "determine if a singly-linked list is a palindrome" in {
    val data = Table(
      ("xs", "palindrome"),
      (Seq[Integer](3, 2, 1, 1, 2, 3).asJava, true),
      (Seq[Integer](1, 2, 3).asJava, false),
      (Seq[Integer](1, 1).asJava, true),
      (Seq[Integer](1).asJava, true),
      (Seq.empty[Integer].asJava, false),
      (Seq[Integer](2, 1, 1, 1, 1).asJava, false),
      (Seq[Integer](1, 1, 1, 1).asJava, true),
      (Seq[Integer](3, 2, 0, 1, 2, 3).asJava, false),
      (Seq[Integer](3, 2, 1, 0, 1, 2, 3).asJava, true),
      (Seq[Integer](1, 1, 1).asJava, true)
    )

    forAll(data) { (xs, palindrome) =>
      Lists.isPalindrome(new Node[Integer](xs)) shouldBe palindrome
    }
    Lists.isPalindrome(null) shouldBe false
  }
}

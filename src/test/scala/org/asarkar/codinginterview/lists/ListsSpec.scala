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
      (Seq.empty[Integer].asJava, true),
      (Seq[Integer](2, 1, 1, 1, 1).asJava, false),
      (Seq[Integer](1, 1, 1, 1).asJava, true),
      (Seq[Integer](3, 2, 0, 1, 2, 3).asJava, false),
      (Seq[Integer](3, 2, 1, 0, 1, 2, 3).asJava, true),
      (Seq[Integer](1, 1, 1).asJava, true)
    )

    forAll(data) { (xs, palindrome) =>
      Lists.isPalindrome(new Node[Integer](xs)) shouldBe palindrome
    }
    Lists.isPalindrome(null) shouldBe true
  }

  it should "add two lists representing integers" in {
    Lists.addTwoNumbers(
      new Node[Integer](Seq[Integer](2, 4, 3).asJava),
      new Node[Integer](Seq[Integer](5, 6, 4).asJava)
    ).toList.asScala should contain theSameElementsInOrderAs Seq(7, 0, 8)

    Lists.addTwoNumbers(
      new Node[Integer](Seq[Integer](9).asJava),
      new Node[Integer](Seq[Integer](9).asJava)
    ).toList.asScala should contain theSameElementsInOrderAs Seq(8, 1)

    Lists.addTwoNumbers(
      new Node[Integer](Seq[Integer](1).asJava),
      new Node[Integer](Seq[Integer](11).asJava)
    ).toList.asScala should contain theSameElementsInOrderAs Seq(2, 1)
  }

  it should "rearrange a list to alternate high-low" in {
    Lists.loHi(
      new Node[Integer](Seq[Integer](1, 2, 3, 4, 5, 6, 7).asJava)
    ).toList.asScala should contain theSameElementsInOrderAs Seq(1, 3, 2, 5, 4, 7, 6)

    Lists.loHi(
      new Node[Integer](Seq[Integer](9, 6, 8, 3, 7).asJava)
    ).toList.asScala should contain theSameElementsInOrderAs Seq(6, 9, 3, 8, 7)

    Lists.loHi(
      new Node[Integer](Seq[Integer](6, 9, 2, 5, 1, 4).asJava)
    ).toList.asScala should contain theSameElementsInOrderAs Seq(6, 9, 2, 5, 1, 4)
  }

  it should "find the intersection of two lists" in {
    val common: Node[Integer] = new Node[Integer](Seq[Integer](8, 4, 5).asJava)
    val l1 = new Node[Integer](new Node[Integer](common, 1), 4)
    val l2 = new Node[Integer](new Node[Integer](new Node[Integer](common, 1), 0), 5)
    Lists.getIntersectionNode(l1, l2).toList.asScala should contain theSameElementsInOrderAs Seq(8, 4, 5)

    Lists.getIntersectionNode(new Node[Integer](1), new Node[Integer](2)) shouldBe null
  }

  it should "merge two sorted lists" in {
    val l1: Node[Integer] = new Node[Integer](Seq[Integer](1, 3, 4).asJava)
    val l2: Node[Integer] = new Node[Integer](Seq[Integer](1, 2, 4).asJava)

    Lists.merge2Lists(l1, l2).toList.asScala shouldBe sorted
    Lists.merge2Lists(new Node[Integer](null, 0), null).toList.asScala shouldBe sorted
  }

  it should "merge k sorted lists" in {
    val l1: Node[Integer] = new Node[Integer](Seq[Integer](1, 4, 5).asJava)
    val l2: Node[Integer] = new Node[Integer](Seq[Integer](1, 3, 4).asJava)
    val l3: Node[Integer] = new Node[Integer](Seq[Integer](2, 6).asJava)

    Lists.mergeKLists(Array(l1, l2, l3)).toList.asScala shouldBe sorted
  }
}

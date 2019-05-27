package org.asarkar.codinginterview.stacksnqueues

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class QueueSpec extends FlatSpec {
  "queue" should "enqueue and dequeue" in {
    val q = new Queue[Int]()
    (1 to 3).foreach(q.enqueue)

    q.dequeue() shouldBe 1
    q.dequeue() shouldBe 2
    q.enqueue(1)
    q.dequeue() shouldBe 3
    q.dequeue() shouldBe 1

    assertThrows[NoSuchElementException] {
      q.dequeue()
    }
  }
}

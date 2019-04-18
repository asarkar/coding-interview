package org.asarkar.codinginterview.design

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.prop.TableDrivenPropertyChecks

class LfuCacheSpec extends FlatSpec with TableDrivenPropertyChecks {
  "LruCache" should "get nonexistent item" in {
    val cache = LfuCache[Int, Int]()
    cache.isEmpty shouldBe true
    cache.get(0) shouldBe empty
  }

  it should "set and get" in {
    val cache = new LfuCache[Int, Int](2)
    cache.set(0, 1000) shouldBe empty
    cache.get(0).value shouldBe 1000
    cache.size shouldBe 1
  }

  it should "update and get" in {
    val cache = new LfuCache[Int, Int](2)
    cache.set(0, 1000) shouldBe empty
    cache.set(0, 1001).value shouldBe(0, 1000)
    cache.size shouldBe 1
  }

  it should "evict the lfu item" in {
    val cache = new LfuCache[Int, Int](10)
    object CacheOp extends Enumeration {
      val Set, Get = Value
    }

    val data = Table(
      ("op", "k", "v"),
      (CacheOp.Set, 10, 13),
      (CacheOp.Set, 3, 17),
      (CacheOp.Set, 6, 11),
      (CacheOp.Set, 10, 5),
      (CacheOp.Set, 9, 10),
      (CacheOp.Get, 13, -1),
      (CacheOp.Set, 2, 19),
      (CacheOp.Get, 2, 19),
      (CacheOp.Get, 3, 17),
      (CacheOp.Set, 5, 25),
      (CacheOp.Get, 8, -1),
      (CacheOp.Set, 9, 22),
      (CacheOp.Set, 5, 5),
      (CacheOp.Set, 1, 30),
      (CacheOp.Get, 11, -1),
      (CacheOp.Set, 9, 12),
      (CacheOp.Get, 7, -1),
      (CacheOp.Get, 5, 5),
      (CacheOp.Get, 8, -1),
      (CacheOp.Get, 9, 12),
      (CacheOp.Set, 4, 30),
      (CacheOp.Set, 9, 3),
      (CacheOp.Get, 9, 3),
      (CacheOp.Get, 10, 5),
      (CacheOp.Get, 10, 5),
      (CacheOp.Set, 6, 14),
      (CacheOp.Set, 3, 1),
      (CacheOp.Get, 3, 1),
      (CacheOp.Set, 10, 11),
      (CacheOp.Get, 8, -1),
      (CacheOp.Set, 2, 14),
      (CacheOp.Get, 1, 30),
      (CacheOp.Get, 5, 5),
      (CacheOp.Get, 4, 30),
      (CacheOp.Set, 11, 4),
      (CacheOp.Set, 12, 24),
      (CacheOp.Set, 5, 18),
      (CacheOp.Get, 13, -1),
      (CacheOp.Set, 7, 23),
      (CacheOp.Get, 8, -1),
      (CacheOp.Get, 12, 24),
      (CacheOp.Set, 3, 27),
      (CacheOp.Set, 2, 12),
      (CacheOp.Get, 5, 18),
      (CacheOp.Set, 2, 9),
      (CacheOp.Set, 13, 4),
      (CacheOp.Set, 8, 18),
      (CacheOp.Set, 1, 7),
      (CacheOp.Get, 6, 14),
      (CacheOp.Set, 9, 29),
      (CacheOp.Set, 8, 21),
      (CacheOp.Get, 5, 18),
      (CacheOp.Set, 6, 30),
      (CacheOp.Set, 1, 12),
      (CacheOp.Get, 10, 11),
      (CacheOp.Set, 4, 15),
      (CacheOp.Set, 7, 22),
      (CacheOp.Set, 11, 26),
      (CacheOp.Set, 8, 17),
      (CacheOp.Set, 9, 29),
      (CacheOp.Get, 5, 18),
      (CacheOp.Set, 3, 4),
      (CacheOp.Set, 11, 30),
      (CacheOp.Get, 12, -1),
      (CacheOp.Set, 4, 29),
      (CacheOp.Get, 3, 4),
      (CacheOp.Get, 9, 29),
      (CacheOp.Get, 6, 30),
      (CacheOp.Set, 3, 4),
      (CacheOp.Get, 1, 12),
      (CacheOp.Get, 10, 11),
      (CacheOp.Set, 3, 29),
      (CacheOp.Set, 10, 28),
      (CacheOp.Set, 1, 20),
      (CacheOp.Set, 11, 13),
      (CacheOp.Get, 3, 29),
      (CacheOp.Set, 3, 12),
      (CacheOp.Set, 3, 8),
      (CacheOp.Set, 10, 9),
      (CacheOp.Set, 3, 26),
      (CacheOp.Get, 8, 17),
      (CacheOp.Get, 7, -1),
      (CacheOp.Get, 5, 18),
      (CacheOp.Set, 13, 17),
      (CacheOp.Set, 2, 27),
      (CacheOp.Set, 11, 15),
      (CacheOp.Get, 12, -1),
      (CacheOp.Set, 9, 19),
      (CacheOp.Set, 2, 15),
      (CacheOp.Set, 3, 16),
      (CacheOp.Get, 1, 20),
      (CacheOp.Set, 12, 17),
      (CacheOp.Set, 9, 1),
      (CacheOp.Set, 6, 19),
      (CacheOp.Get, 4, 29),
      (CacheOp.Get, 5, 18),
      (CacheOp.Get, 5, 18),
      (CacheOp.Set, 8, 1),
      (CacheOp.Set, 11, 7),
      (CacheOp.Set, 5, 2),
      (CacheOp.Set, 9, 28),
      (CacheOp.Get, 1, 20),
      (CacheOp.Set, 2, 2),
      (CacheOp.Set, 7, 4),
      (CacheOp.Set, 4, 22),
      (CacheOp.Set, 7, 24),
      (CacheOp.Set, 9, 26),
      (CacheOp.Set, 13, 28),
      (CacheOp.Set, 11, 26)
    )

    forAll(data) { (op, k, v) =>
      if (op == CacheOp.Set) cache.set(k, v)
      else cache.get(k).getOrElse(-1) shouldBe v
    }
  }
}

package org.asarkar.codinginterview.design

import org.scalatest.Matchers._
import org.scalatest.{FlatSpec, OptionValues}

class LruCacheSpec extends FlatSpec with OptionValues {

  implicit class OptionalOps[A](opt: java.util.Optional[A]) {
    def toOption: Option[A] = if (opt.isPresent) Some(opt.get()) else None
  }

  "LruCache" should "get nonexistent item" in {
    val cache = new LruCache[Int, Int](2)
    cache.isEmpty shouldBe true

    cache.get(0).toOption shouldBe empty
  }

  it should "set and get" in {
    val cache = new LruCache[Int, Int](2)
    cache.set(0, 1000).toOption shouldBe empty
    cache.get(0).toOption.value shouldBe 1000
    cache.size() shouldBe 1
  }

  it should "update and get" in {
    val cache = new LruCache[Int, Int](2)
    cache.set(0, 1000).toOption shouldBe empty
    cache.set(0, 1001).toOption.value shouldBe 1000
    cache.size() shouldBe 1
  }

  it should "evict the oldest item" in {
    val cache = new LruCache[Int, Int](2)
    cache.set(0, 1000).toOption shouldBe empty
    cache.set(1, 1001).toOption shouldBe empty
    cache.set(2, 1002).toOption shouldBe empty

    cache.get(0).toOption shouldBe empty
    cache.size() shouldBe 2
  }

  it should "delete" in {
    val cache = new LruCache[Int, Int](2)
    cache.set(0, 1000).toOption shouldBe empty
    cache.delete(0).toOption.value shouldBe 1000
    cache.get(0).toOption shouldBe empty
    cache.isEmpty shouldBe true
  }
}

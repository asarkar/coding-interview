package org.asarkar.codinginterview.design

import scala.collection.mutable

/*
 * Implement an LFU (Least Frequently Used) cache. It should be able to be initialized with a cache size n, and contain
 * the following methods:
 * - set(key, value): sets key to value. If there are already n items in the cache and we are adding a new item, then
 * it should also remove the least frequently used item. If there is a tie, then the least recently used key should be
 * removed.
 * - get(key): gets the value at key. If no such key exists, return null.
 *
 * Each operation should run in O(1) time.
 *
 * ANSWER: We need to keep track of three things:
 * 1. key -> value: This is a cache after all.
 * 2. Key -> access frequency: Obviously we need to know how many times a key is accessed (set and get).
 * 3. Least accessed keys: This is simple using a sorted map, but sorted maps are implemented using balanced BSTs,
 *    which provide log(n) time operations and we are shooting for O(1). The key observation to make is that we don't
 *    need a total ordering on all the frequencies, but just need to know the minimum one.
 *
 * Last but not the least, multiple keys could be accessed same number of times. To break ties in case of evictions,
 * the list of keys corresponding to each frequency are maintained in the insertion order. Thus, the last one is the
 * one most recently accessed, and the first one, least recently.
 *
 * With the above setup, the algorithm boils down to careful map manipulation. When a key is accessed, we increment
 * its access count, remove it from the list corresponding to the existing access count, and append to the list
 * corresponding to the incremented access count.
 */
class LfuCache[K, V](val capacity: Int) {
  require(capacity > 0, "Capacity must be positive")

  private val kv = mutable.Map.empty[K, V]
  private val freq = mutable.Map.empty[K, Int]
  private val revFreq = mutable.Map.empty[Int, mutable.LinkedHashSet[K]]
  private var lfu = Int.MaxValue

  def isEmpty: Boolean = kv.isEmpty

  def size: Int = kv.size

  /**
    * @param k key
    * @return optionally the value corresponding to this key
    */
  def get(k: K): Option[V] = {
    val v = kv.get(k)
    v.foreach(_ => updateFreq(k))
    v
  }

  /**
    * @param k key
    * @param v value
    * @return if the cache is full and doesn't contain the key, the entry that's evicted; otherwise, optionally
    *         the previous mapping corresponding to this key
    */
  def set(k: K, v: V): Option[(K, V)] = {
    val prev = if (!kv.contains(k) && kv.size == capacity) {
      revFreq(lfu).headOption
        .flatMap { ev =>
          revFreq(lfu).remove(ev)
          freq.remove(ev)
          kv.remove(ev).map { v1 =>
            println(s"Evicted: ($ev, $v1), inserted: ($k, $v)")
            (ev, v1)
          }
        }
    } else kv.get(k).map((k, _))

    updateFreq(k)
    kv(k) = v

    prev
  }

  private def updateFreq(k: K): Unit = {
    val count = freq.getOrElse(k, 0)
    freq.update(k, count + 1)

    lfu = math.min(lfu, count + 1)

    revFreq.get(count).foreach(_.remove(k))
    if (revFreq.contains(count) && revFreq(count).isEmpty) {
      revFreq.remove(count)
      if (lfu == count) lfu += 1
    }
    revFreq.get(count + 1) match {
      case Some(keys) => keys.add(k)
      case _ => revFreq.update(count + 1, mutable.LinkedHashSet(k))
    }
  }
}

object LfuCache {
  def apply[K, V](capacity: Int = 10): LfuCache[K, V] = new LfuCache(capacity)
}

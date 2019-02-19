package org.asarkar.codinginterview.design;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/*
 * A 'get' is equivalent to deleting the corresponding element, and inserting a new node at the front with the
 * same value.
 * A 'put' is equivalent to deleting the corresponding element, if present, or the last element if the cache is full,
 * and inserting a new node at the front with the given value.
 * A 'delete' does exactly what the name suggests.
 */
public class LruCache<K, V> {
    private final Map<K, ListNode<K, V>> map;
    private final int capacity;
    private K first;
    private K last;

    public LruCache(int capacity) {
        this.capacity = capacity;
        this.map = new HashMap<>();
    }

    /**
     * @return True if the cache is currently empty, false otherwise
     */
    public boolean isEmpty() {
        return map.isEmpty();
    }

    /**
     * @return Number of items in the cache
     */
    public int size() {
        return map.size();
    }

    /**
     * @param key Key
     * @return True if the cache contains an items corresponding to the given key, false otherwise
     */
    public boolean contains(K key) {
        return map.containsKey(key);
    }

    /**
     * @param key Key
     * @return Associated value, if the item is present in the cache
     */
    public Optional<V> get(K key) {
        if (contains(key)) {
            return set(key, map.get(key).value);
        }

        return Optional.empty();
    }

    /**
     * @param key Key
     * @return Associated value, if the item was present in the cache
     */
    public Optional<V> delete(K key) {
        if (!contains(key)) {
            return Optional.empty();
        }

        ListNode<K, V> entry = map.remove(key);

        if (last == first) {
            first = null;
            last = null;
        } else if (key == first) {
            first = map.get(first).next;
        } else {
            map.get(entry.prev).next = entry.next;
        }
        return Optional.of(entry.value);
    }

    /**
     * @param key   Key
     * @param value Value
     * @return Previous value, if the item was present in the cache
     */
    public Optional<V> set(K key, V value) {
        if (value == null) {
            throw new IllegalArgumentException("Value must not be null");
        }
        Optional<V> old = Optional.empty();
        if (contains(key)) {
            old = delete(key);
        } else if (size() == capacity) {
            delete(last);
        }

        ListNode<K, V> entry = new ListNode<>(null, first, value);
        if (isEmpty()) {
            last = key;
        } else {
            map.get(first).prev = key;
        }
        first = key;
        map.put(key, entry);

        return old;
    }

    private static class ListNode<K, V> {
        private K prev;
        private K next;
        private V value;

        private ListNode(K prev, K next, V value) {
            this.prev = prev;
            this.next = next;
            this.value = value;
        }
    }
}

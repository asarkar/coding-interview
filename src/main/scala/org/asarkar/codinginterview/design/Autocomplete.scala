package org.asarkar.codinginterview.design

import scala.collection.mutable

case class TrieNode(
                     children: mutable.Map[Char, TrieNode] = mutable.Map.empty,
                     suggestions: mutable.Set[String] = mutable.Set.empty,
                     end: Boolean = false
                   ) {
  def insert(word: String): Unit = {
    insert(word, 0)
  }

  def contains(word: String): Boolean = find(word).isDefined

  def find(word: String): Option[TrieNode] = {
    if (!children.contains(word.headOption.getOrElse('\u0000'))) None
    else if (word.tail.isEmpty) children.get(word.head)
    else children.get(word.head)
      .flatMap(_.find(word.tail))
  }

  private def insert(word: String, i: Int): Unit = {
    if (word.isDefinedAt(i)) {
      val ch = word(i)
      if (!children.contains(ch))
        children += (ch -> TrieNode(end = i == word.length - 1))

      children(ch).suggestions += word
      children(ch).insert(word, i + 1)
    }
  }
}

/*
 * Implement an autocomplete system. That is, given a query string s and a set of all possible query strings,
 * return all strings in the set that have s as a prefix.
 * For example, given the query string de and the set of strings [dog, deer, deal], return [deer, deal].
 *
 * Hint: Try preprocessing the dictionary into a more efficient data structure to speed up queries.
 *
 * ANSWER: Trie is the optimal data structure for prefix matching, but using a naive Trie, we would have to find
 * all leaves reachable from the node corresponding to the last character in the query string. This may take O(N) time
 * in the worst case, where N is the number of leaves in the Trie. Can we do better?
 *
 * We can, at the expense of additional space. At each node, we store all the suggestions that have prefixes matching
 * up to this node. For example, for a Trie root -> c -> a -> t, we store the word "cat" at each of the nodes 'c', 'a'
 * and 't'. If we insert another word "cut", node 'c' now stores suggestions "cat" and "cut", 'u' stores "cut" and 't'
 * stores "cut".
 * With this modification, we can return all suggestions in O(L) time, where L is the length of the query.
 *
 * Further optimizations and improvements are possible. See https://prefixy.github.io/ and
 * https://www.youtube.com/watch?v=us0qySiUsGU.
 */
class Autocomplete(val words: Seq[String]) {
  private val trie = TrieNode()

  words
    .foreach(trie.insert)

  def suggestions(prefix: String): Seq[String] = {
    trie
      .find(prefix)
      .map(_.suggestions.toSeq)
      .getOrElse(Seq.empty)
  }
}

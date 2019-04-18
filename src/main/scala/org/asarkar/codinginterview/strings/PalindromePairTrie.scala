package org.asarkar.codinginterview.strings

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer, Map => MutableMap}

class PalindromePairTrie {

  case class TrieNode(
                       children: MutableMap[Char, TrieNode] = MutableMap.empty,
                       palin: ListBuffer[Int] = ListBuffer.empty
                     )

  private val root = TrieNode()

  def +=(word: String, index: Int): PalindromePairTrie = {
    put(root, word, word.length - 1, index)
    this
  }

  @tailrec
  private def put(node: TrieNode, word: String, i: Int, index: Int): Unit = {
    if (i >= 0) {
      val child = node.children.getOrElseUpdate(word(i), TrieNode())
      if (isPalindrome(word, 0, i - 1)) {
        child.palin += index
      }
      put(child, word, i - 1, index)
    }
  }

  def isPalindrome(word: String, lo: Int, hi: Int): Boolean = {
    lo >= hi ||
      word(lo) == word(hi) && isPalindrome(word, lo + 1, hi - 1)
  }

  def get(word: String): Option[TrieNode] = {
    get(root, word, 0)
  }

  private def get(node: TrieNode, word: String, i: Int): Option[TrieNode] = {
    if (word.isEmpty) None
    else {
      val child = node.children.get(word(i))
      if (i == (word.length - 1)) child
      else child.flatMap(get(_, word, i + 1))
    }
  }
}

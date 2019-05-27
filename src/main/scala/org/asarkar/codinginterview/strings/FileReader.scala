package org.asarkar.codinginterview.strings

/*
 * Using a read7() method that returns 7 characters from a file, implement readN(n) which reads n characters.
 * For example, given a file with the content "Hello world", three read7() returns "Hello w", "orld" and then "".
 */
class FileReader(content: String) {
  private var offset = 0

  def read7(): String = {
    if (offset >= content.length) ""
    else {
      val s = content.substring(offset, offset + math.min(7, content.length - offset))
      offset += s.length
      s
    }
  }

  def readN(n: Int): String = {
    val times = math.ceil(math.max(n, content.length) / 7d).toInt
    val offsetBefore = offset

    val s = Iterator.continually(read7())
      .take(times)
      .mkString
      .take(n)

    offset = offsetBefore + s.length

    s
  }
}

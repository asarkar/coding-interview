package org.asarkar.codinginterview.design

class JsonFormatter {
  private val Space = " "
  private val Echo = (c: Char, buffer: StringBuilder) => buffer.append(c)
  private val Enter = (_: Char, buffer: StringBuilder) => buffer.append(System.lineSeparator())
  private val LevelUp = (_: Char, _: StringBuilder) => level += 1
  private val LevelDown = (_: Char, _: StringBuilder) => level -= 1
  private val Indent = (_: Char, buffer: StringBuilder) => buffer.appendAll(Space * (4 * level))
  private val Rules = Map(
    '{' -> Seq(Echo, Enter, LevelUp, Indent),
    '[' -> Seq(Echo, Enter, LevelUp, Indent),
    ',' -> Seq(Echo, Enter, Indent),
    '}' -> Seq(Enter, LevelDown, Indent, Echo),
    ']' -> Seq(Enter, LevelDown, Indent, Echo)
  )
  private var level: Int = 0

  def format(s: String): String = {
    val buffer = new StringBuilder()

    s
      .foreach { c =>
        if (Rules.contains(c)) Rules(c).foreach(f => f(c, buffer))
        else if (c.isWhitespace) {} // skip
        else Echo(c, buffer)
      }

    level = 0
    buffer.toString()
  }
}

object JsonFormatter {
  def main(args: Array[String]): Unit = {
    println(new JsonFormatter().format(
      """{"name": "John","age":"40","children":[{"name": "Bob","age":"17"},{"name": "Maria","age":"8"}]}"""
    ))

    println(new JsonFormatter().format(
      """["foo", {"bar":["baz",null,1.0,2]}]"""
    ))
  }
}

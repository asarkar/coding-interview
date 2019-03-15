package org.asarkar.codinginterview.lists

sealed trait LinkedList[+A] {
  def datum: A = ???

  def next: LinkedList[A] = ???

  def nonEmpty: Boolean = !isEmpty

  def isEmpty: Boolean = true

  def toSeq: Seq[A] = foldLeft(Seq.empty[A])((acc, x) => acc :+ x)

  def foldLeft[B](acc: B)(f: (B, A) => B): B = acc
}

case object Nil extends LinkedList[Nothing]

case class Cons[+A](
                     override val datum: A,
                     override val next: LinkedList[A],
                   ) extends LinkedList[A] {
  override def isEmpty: Boolean = false

  override def foldLeft[B](acc: B)(f: (B, A) => B): B = {
    val current = f(acc, datum)
    next.foldLeft(current)(f)
  }
}

object LinkedList {
  def apply[A](xs: A*): LinkedList[A] = xs.foldRight(empty[A])((x, acc) => LinkedList(x, acc))

  def apply[A](datum: A, next: LinkedList[A]): LinkedList[A] = Cons(datum, next)

  def empty[A]: LinkedList[A] = Nil
}

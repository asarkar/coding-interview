package org.asarkar.codinginterview.graphs

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TopSort[A](private val graph: collection.Map[A, collection.Set[A]], private val vertices: Seq[A]) {
  val stack: ListBuffer[A] = ListBuffer.empty[A]

  private object VertexState extends Enumeration {
    val NotVisited, Visiting, Visited = Value
  }

  private val visited = mutable.Map.empty[A, VertexState.Value]

  private def hasLoop(v: A): Boolean = {
    val vState = visited.getOrElseUpdate(v, VertexState.NotVisited)

    if (vState == VertexState.Visiting) true
    else if (vState == VertexState.Visited) false
    else {
      visited.update(v, VertexState.Visiting)
      val loop = graph
        .getOrElse(v, mutable.Set.empty[A])
        .view
        .exists(hasLoop)
      if (!loop) {
        v +=: stack
        visited.update(v, VertexState.Visited)
      }
      loop
    }
  }

  val hasCycle: Boolean = vertices.exists(hasLoop)

  if (hasCycle) {
    stack.clear()
    visited.clear()
  }
}

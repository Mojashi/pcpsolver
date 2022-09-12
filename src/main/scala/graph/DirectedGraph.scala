package graph

import scala.collection.mutable.{ArrayBuffer, Map as MutableMap}

type EdgeId = Int
trait EdgeLike[V](from: V, to: V, id: EdgeId) {}

case class Edge[V](from: V, to: V, id: EdgeId) extends EdgeLike[V](from, to, id)

class DirectedGraph[V]
(
  edges: Map[V, List[Edge[V]]]
) {

  def eulerTrail(start: V, useCount: Map[EdgeId, Int]): List[Edge[V]] = {
      val used = MutableMap[EdgeId, Int]()
      val trail = ArrayBuffer[Edge[V]]()

      def dfs(cur: V, fromEdge: Option[Edge[V]]): Unit = {
        edges.getOrElse(cur, List()).foreach(edge => {
          if(used.getOrElseUpdate(edge.id, 0) < useCount.getOrElse(edge.id, 0)) {
            used(edge.id) += 1
            dfs(edge.to, Some(edge))
          }
        })
        fromEdge match
          case Some(e) => trail.append(e)
          case None =>
      }

      dfs(start, None)

      trail.reverse.toList
  }
}

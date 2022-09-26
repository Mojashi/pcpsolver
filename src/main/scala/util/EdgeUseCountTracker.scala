package util
import graph.EdgeId
import presburger.*

import scala.collection.mutable.{Map as MutableMap, ListBuffer}

class EdgeUseCountTracker {
  val parts = MutableMap[Variable, ListBuffer[Variable]]()
  def AddPart(original: Variable, part: Variable) = {
    parts.getOrElseUpdate(original, ListBuffer()).addOne(part)
  }
  def formulaFor(v: Variable) = parts.get(v).flatMap(ps=>
      Some(Equal(
        v,
        ps.toSeq.reduce(Add.apply)
      ))
  )

  def formula = AndList(
    parts.toMap.map((orig, ps) =>
      Equal(
        orig,
        ps.toSeq.reduce(Add.apply)
      )
    ).toSeq
  )
}

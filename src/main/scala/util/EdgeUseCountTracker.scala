package util
import graph.EdgeId
import presburger.*

import scala.collection.mutable.{Map as MutableMap, ListBuffer}

class EdgeUseCountTracker {
  val parts = MutableMap[Variable, ListBuffer[Variable]]()
  def AddPart(original: Variable, part: Variable) = {
    parts.getOrElseUpdate(original, ListBuffer()).addOne(part)
  }
  def formula(usableVars: Set[VarName]) = AndList(
    parts.toMap.map((orig, ps) =>
      Equal(
        orig,
        ps.filter(p=>usableVars.contains(p.name)).toSeq.fold(Constant(0))(Add.apply)
      )
    ).toSeq
  )
}

package util
import graph.EdgeId
import presburger.*

import scala.collection.mutable.{Map as MutableMap, ListBuffer}

class EdgeUseCountTracker {
  class Session{
    val parts = MutableMap[Variable, ListBuffer[Variable]]()

    def AddPart(original: Variable, part: Variable) = {
      parts.getOrElseUpdate(original, ListBuffer()).addOne(part)
    }

    def formula(usableVars: Set[VarName]) = AndList(
      parts.toMap.map((orig, ps) =>
        Equal(
          orig,
          ps.filter(p => usableVars.contains(p.name)).toSeq.fold(Constant(0))(Add.apply)
        )
      ).toSeq
    )
  }
  val sessions = ListBuffer[Session]()

  def newSession() = {
    sessions.addOne(Session())
  }

  def AddPart: (Variable, Variable) => ListBuffer[Variable] = sessions.last.AddPart
  def formula(usableVars: Set[VarName]): ExistentialPresburgerFormula = {
    var formula: ExistentialPresburgerFormula = True
    var vars = usableVars
    sessions.reverse.foreach(s => {
      formula = And(formula, s.formula(vars))
      vars = vars.union(formula.enumerateVar)
    })
    formula
  }
}

def limitDomain(f: ExistentialPresburgerFormula, usableVars: Set[VarName]): ExistentialPresburgerFormula = {
  f.map{pe => pe match
    case Variable(name) => if(usableVars.contains(name)) pe else Constant(0)
    case _ => pe
  }
}
package util
import graph.EdgeId
import presburger.*
import math.Numeric.Implicits.infixNumericOps

import math.Ordered.orderingToOrdered
import math.Ordering.Implicits.infixOrderingOps
import scala.collection.mutable.{Map as MutableMap, ListBuffer}

class EdgeUseCountTracker {
  class Session{
    val parts = MutableMap[VarName, ListBuffer[VarName]]()

    def AddPart(original: VarName, part: VarName) = {
      parts.getOrElseUpdate(original, ListBuffer()).addOne(part)
    }


    def calc[T : Numeric](mp: Map[VarName, T]): Map[VarName, T] = {
      val cmp = MutableMap.from(mp)

      parts.toMap.foreach((v, ps) => {
        val oo = ps.map(p => mp.getOrElse(p, implicitly[Numeric[T]].zero)).sum
        cmp.get(v) match {
          case Some(a) => {
            assert((a - oo).toDouble < 0.001)
//            println(s"$v $ps: $a, $oo")
          }
          case None => cmp(v) = oo
        }
      })

      cmp.toMap
    }

    def formula(usableVars: Set[VarName]) = AndList(
      parts.toMap.map((orig, ps) =>
        Equal(
          Variable(orig),
          ps.filter(p => usableVars.contains(p)).map(v=>Variable(v)).fold(Constant(0))(Add.apply)
        )
      ).toSeq
    )
  }
  val sessions = ListBuffer[Session]()

  def newSession() = {
    sessions.addOne(Session())
  }

  def AddPart (a:VarName, b:VarName) = sessions.last.AddPart(a,b)

  def calc[T : Numeric](mp: Map[VarName, T]): Map[VarName, T] = {
    var cmp = mp
    sessions.reverse.foreach(s => {
      cmp = s.calc(cmp)
    })
    cmp
  }

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
  f.map{
    case Variable(name) => if (usableVars.contains(name)) Variable(name) else Constant(0)
    case other => other
  }
}
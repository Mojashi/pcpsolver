package automaton

import com.google.ortools.linearsolver.{MPSolver, MPVariable}
import dataType.*
import graph.{EdgeId, EdgeUseCountVar}
import presburger.*
import transducer.Transducer
import util.{MPContext, MPInf, MPNegInf, PrettyPrintMap, timer}

import scala.collection.mutable.ListBuffer


class ParikhAutomaton[State, Key]
(
  start: State,
  fin: Set[State],
  transitions: Seq[Transition[State, IntVector[Key]]],
)(
  implicit outM: Monoid[IntVector[Key]]
) extends EPSFreeNFA[State, IntVector[Key]] (
  start, fin, transitions
) {

  val keys: Set[Key] = transitions.flatMap(t=>t.in.keys).toSet

  def prefixForKeyCount = "alphabet_"
  def KeyCountVar: (Key=>Variable) = (key: Key) => Variable(prefixForKeyCount + key)

  val chCountPresburgerFormula: ExistentialPresburgerFormula =
    AndList(keys.toSeq.map[ExistentialPresburgerFormula](key =>
      Equal(
        transitions.map(t =>
          Mul(
            Constant(t.in.getOrElse(key, 0)),
            EdgeUseCountVar(t.id)
          )
        ).reduce(Add.apply),
        KeyCountVar(key)
      )
    ))


  class MPParikhVars(implicit ctx: MPContext) {
    def chCountVar(key: Key) = ctx.vars.getOrElseUpdate(key.toString, ctx.solver.makeNumVar(MPNegInf, MPInf, KeyCountVar(key).name))
  }

  def calcParikhImageForLP(implicit ctx: MPContext) = {
    val parikhVars = MPParikhVars()
    val graphVars = MPGraphVars()

    keys.foreach(key => {
      val cons = ctx.solver.makeConstraint(0, 0)

      epsFreeTransitions
        .filter(t => t.in.contains(key))
        .foreach(t => {
          cons.setCoefficient(graphVars.edgeUseCountVars(t.id), t.in(key))
        })
      cons.setCoefficient(parikhVars.chCountVar(key), -1)
    })
  }
}


def solveParikhImageToZeroWithLP[State, Key](pnfa: ParikhAutomaton[State, Key], exact: Boolean): Option[Map[EdgeId, Int]] = {
  val solver = MPSolver.createSolver(if(exact) "SCIP" else "GLOP")

  implicit val ctx = MPContext(
    solver = solver,
    integerEnabled = exact,
  )
  val nn = pnfa.setSingleFin

  val ivm = IntVectorMonoid[Key]()

  val normalized = ParikhAutomaton(nn.start, nn.fin, nn.transitions.map(t =>
    Transition(t.from, t.to, t.in.getOrElse(ivm.unit) , t.id)
  ))(ivm)


  val graphVars = normalized.MPGraphVars()

  val effectivePathCons = solver.makeConstraint(1, MPInf)

  val obj = solver.objective()

  normalized.transitions
    .filter(t => t.from != normalized.start || t.to != normalized.fin.head)
    .map(t => t.id)
    .foreach(eid => {
      effectivePathCons.setCoefficient(graphVars.edgeUseCountVars(eid), 1)
//      obj.setCoefficient(graphVars.edgeUseCountVars(eid), 1)
    })
  effectivePathCons.setIsLazy(true)
  obj.setMinimization()

  normalized.flowConstraintInLP(normalized.start, normalized.fin.head)
  normalized.calcParikhImageForLP
  val parikhVars = normalized.MPParikhVars()

  normalized.keys.foreach(key => {
    parikhVars.chCountVar(key).setLb(0)
    parikhVars.chCountVar(key).setUb(0)
  })

  var retry = 0
  while(true) {
    val result = timer {
      solver.solve()
    }

    if (result != MPSolver.ResultStatus.FEASIBLE && result != MPSolver.ResultStatus.OPTIMAL) {
      return None
    }

    val resValue = ctx.vars.map((k, v: MPVariable) => (k,
      if(exact) v.solutionValue().intValue()
      else Math.round(v.solutionValue()).toInt
    ))

    println(normalized.transitions.map(t => graphVars.edgeUseCountVars(t.id).solutionValue()).toSet)

    val edgeUseCount = normalized.extractEdgeUseCount(resValue.toMap)
    // print(edgeUseCount.prettyPrint)

    val connected = normalized.isConnected(edgeUseCount)
    println(s"connected: ${connected}")
    if (connected) {
      return Some(pnfa.extractEdgeUseCount(resValue.toMap))
    }

//    normalized.saveSVG(s"normalized_lp_$retry", "", edgeUseCount)
    timer {
      normalized.connectivityConstraintInLP(normalized.start, edgeUseCount)
//      normalized.connectivityConstraintInLPWithIndegreeEQ(normalized.start, edgeUseCount)
    }
    retry += 1
    println(s"retry: $retry")
  }

  assert(false)
}

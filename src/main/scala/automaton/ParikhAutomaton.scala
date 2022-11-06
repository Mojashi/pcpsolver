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
      val cons = ctx.solver.makeConstraint(0, 0, s"parikhcons_for_${key}")

      epsFreeTransitions
        .filter(t => t.in.contains(key))
        .foreach(t => {
          cons.setCoefficient(graphVars.edgeUseCountVars(t.id), t.in(key))
        })
      cons.setCoefficient(parikhVars.chCountVar(key), -1)
    })
  }
}

def createMPContext[State, Key](normalized: ParikhAutomaton[State, Key], exact: Boolean): MPContext = {
  val solver = MPSolver.createSolver(if(exact) "CPLEX_MIP" else "CPLEX_LP")
  solver.setNumThreads(8)

  implicit val ctx = MPContext(
    solver = solver,
    integerEnabled = exact,
  )

  val graphVars = normalized.MPGraphVars()

  val effectivePathCons = solver.makeConstraint(1, MPInf, "effectivePathCons")

  val obj = solver.objective()

  normalized.transitions
    .filter(t => !t.in.get.isEmpty)
    .map(t => t.id)
    .foreach(eid => {
      effectivePathCons.setCoefficient(graphVars.edgeUseCountVars(eid), 1)
      obj.setCoefficient(graphVars.edgeUseCountVars(eid), 1)
    })
  obj.setMinimization()

  normalized.flowConstraintInLP(normalized.start, normalized.fin.head)
  normalized.calcParikhImageForLP
  val parikhVars = normalized.MPParikhVars()

  normalized.keys.foreach(key => {
    parikhVars.chCountVar(key).setLb(0)
    parikhVars.chCountVar(key).setUb(0)
  })

  normalized.sourceFrom(normalized.start).foreach(t=>
    graphVars.edgeUseCountVars(t.id).setInteger(true)
  )
  normalized.fin.foreach(f =>
    normalized.targetTo(f).foreach(t =>
      graphVars.edgeUseCountVars(t.id).setInteger(true)
    )
  )

  ctx
}
//
//def getAnotherSol(ctx: MPContext): Unit = {
//
//}

def solveParikhImageToZeroWithLP[State, Key](pnfa: ParikhAutomaton[State, Key], exact: Boolean, connectivity: Boolean = true): Option[Map[EdgeId, Double]] = {
  implicit val ctx = createMPContext(pnfa, exact)

  var retry = 0
  while(true) {
    val result = timer {
      ctx.solver.solve()
    }


    if (result != MPSolver.ResultStatus.FEASIBLE && result != MPSolver.ResultStatus.OPTIMAL) {
      println(result)
      return None
    }

    val resValue: Map[String, Double] = ctx.vars.map((k, v: MPVariable) => (k,
      if(exact) v.solutionValue()
//      else Math.ceil(v.solutionValue()).toInt
      else v.solutionValue()
    )).toMap

//    println(pnfa.transitions.map(t => graphVars.edgeUseCountVars(t.id).solutionValue()).toSet)

    val edgeUseCount = pnfa.extractEdgeUseCount[Double](resValue)

    val connected = pnfa.isConnected(edgeUseCount)
    println(s"connected: ${connected}")
    if (connected || !connectivity) {
      println(s"obj: ${ctx.solver.objective().value()}")
      return Some(pnfa.extractEdgeUseCount[Double](resValue))
    }

    pnfa.connectivityConstraintInLP(pnfa.start, edgeUseCount)
    retry += 1
    println(s"retry: $retry")
  }

  assert(false)
}


//def findParikhZeroCycle[State, Key](pnfa: ParikhAutomaton[State, Key], exact: Boolean)(implicit ctx: MPContext): Option[Map[EdgeId, Double]] = {
//
//}

def solveParikhImageToZeroWithLPProvided[State, Key](pnfa: ParikhAutomaton[State, Key])(implicit ctx: MPContext): Option[Double] = {
  val graphVars = pnfa.MPGraphVars()

  var retry = 0
  while(true) {
    val result = timer {
      ctx.solver.solve()
    }

    //    println(ctx.solver.exportModelAsMpsFormat())

    if (result != MPSolver.ResultStatus.FEASIBLE && result != MPSolver.ResultStatus.OPTIMAL) {
      println(result)
      return None
    }

    val resValue: Map[String, Double] = ctx.vars.map((k, v: MPVariable) => (k,
      if(ctx.integerEnabled) v.solutionValue()
      //      else Math.ceil(v.solutionValue()).toInt
      else v.solutionValue()
    )).toMap

    println(pnfa.transitions.map(t => graphVars.edgeUseCountVars(t.id).solutionValue()).toSet)

    val edgeUseCount = pnfa.extractEdgeUseCount[Double](resValue)

    val connected = pnfa.isConnected(edgeUseCount)
    println(s"connected: ${connected}")
    if (connected) {
      println(s"obj: ${ctx.solver.objective().value()}")
      return Some(ctx.solver.objective().value())
    }

    pnfa.connectivityConstraintInLP(pnfa.start, edgeUseCount)
    retry += 1
    println(s"retry: $retry")
  }

  assert(false)
}

def powerSet[A](s:TraversableOnce[A]) =
       s.foldLeft(Set(Set.empty[A])) {
          (set, element) => set union (set map (_ + element))
       }
def connectivityAwareSolve[State, Key](pnfa: ParikhAutomaton[State, Key], exact: Boolean = true, exactLowerBound: Boolean = true): Option[Map[EdgeId, Double]] = {
  implicit val ctx = createMPContext(pnfa, exact)
  val graphVars = pnfa.MPGraphVars()
  val flowVars = pnfa.MPConnectivityFlowVars()

  var retry = 0
  def rec(connected: Seq[State], notConnected: Seq[State]): Option[(Double, Map[EdgeId, Double])] = {
    retry += 1
    println(s"retry: ${retry}")
//    println(connected)
//    println(notConnected)

    pnfa.baseFlowConnectedConstraintInLP()

    val result = timer {
      ctx.solver.solve()
    }

    if (result != MPSolver.ResultStatus.FEASIBLE && result != MPSolver.ResultStatus.OPTIMAL) {
      println(result)
      return None
    }

    val resValue: Map[String, Double] = ctx.vars.map((k, v: MPVariable) => (k,
      if (exact) v.solutionValue()
      else v.solutionValue()
    )).toMap

    val edgeUseCount = pnfa.extractEdgeUseCount[Double](resValue)

    val connectedComponents = pnfa.getConnectedComponent(edgeUseCount)
    val roots = connectedComponents.findRoots.diff(Set(connectedComponents.find(pnfa.start)))

    if(roots.size >= 1) {
      println(s"not connected: ${roots.size} components")

      var minObj = MPInf
      var minObjEUC = Map[EdgeId, Double]()

      for(r <- powerSet(roots)) {
        val newAddNonCon = roots.diff(r)
        val newCon = connected ++ r

        flowVars.flowConstraint(pnfa.start).setBounds(-newCon.size, -newCon.size)

        r.foreach(v => flowVars.flowConstraint(v).setBounds(1, 1))
        newAddNonCon.foreach(v =>
          pnfa.targetTo(v).foreach(e => graphVars.edgeUseCountVars(e.id).setUb(0))
        )

        val tmp = rec(newCon, notConnected ++ newAddNonCon)

        r.foreach(v => flowVars.flowConstraint(v).setBounds(0, 0))
        newAddNonCon.foreach(v =>
          pnfa.targetTo(v).foreach(e => graphVars.edgeUseCountVars(e.id).setUb(MPInf))
        )

        if(tmp.isDefined) {
          if(exactLowerBound) {
            if(minObj > tmp.get._1) {
              minObj = Math.min(minObj, tmp.get._1)
              minObjEUC = tmp.get._2
            }
          } else return tmp
        }
      }
      if(!minObjEUC.isEmpty)
        Some((minObj, minObjEUC))
      else
        None
    }
    else {
      println(s"obj: ${ctx.solver.objective().value()}")
      val euc = pnfa.extractEdgeUseCount[Double](resValue)
      Some(ctx.solver.objective().value(), euc)
    }
  }


  rec(Seq(), Seq()).flatMap(o=>Some(o._2))
}


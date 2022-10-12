package graph

import com.google.ortools.graph.MaxFlow
import com.google.ortools.linearsolver.MPSolver
import presburger.*
import util.{MPContext, MPInf,MPNegInf, UnionFind, timer}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map as MutableMap, Set as MutableSet}
import scala.sys.process.{Process, ProcessIO}

type EdgeId = String
trait EdgeLike[V] {
  val from: V
  val to: V
  val id: EdgeId
}

case class Edge[V](from: V, to: V, id: EdgeId) extends EdgeLike[V]

object UniqueEdgeId {
  var c = 0
  def get: EdgeId = {
    val ret = s"uniqueedge_$c"
    c+=1
    ret
  }
}

val EdgeUseCountVar = (t: EdgeId) => Variable(s"y_${t}")

class DirectedGraph[V, E <: EdgeLike[V]]
(
  val edges: Seq[E]
) {
  val states: Set[V] = edges.flatMap(t => Set(t.from, t.to)).toSet

  val edgesMap: Map[V, Seq[E]] = {
    edges.groupBy(t => t.from)
  }

  val edgesMapByTarget: Map[V, Seq[E]] = {
    edges.groupBy(t => t.to)
  }

  val idToedgesMap: Map[EdgeId, E] =
    edges.map(t => (t.id, t)).toMap
    
  def eulerTrail(start: V, useCount: Map[EdgeId, Int]): Option[Seq[E]] = {
      val used = MutableMap[EdgeId, Int]()
      val trail = ArrayBuffer[E]()

      def dfs(cur: V, fromEdge: Option[E]): Unit = {
        edgesMap.getOrElse(cur, List()).foreach(edge => {
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

      if(useCount.forall((id, requiredCount) => used.getOrElse(id, 0) == requiredCount))
        Some(trail.reverse.toSeq)
      else None
  }

  def extractEdgeUseCount(m: VarValueMap): Map[EdgeId, Int] =
    edges.map(trans =>
      (trans.id, m(EdgeUseCountVar(trans.id).name))
    ).toMap

  def sourceFrom(q: V): Seq[E] = {
    edgesMap.getOrElse(q, Seq())
  }

  def targetTo(q: V): Seq[E] = {
    edgesMapByTarget.getOrElse(q, Seq())
  }

  val distanceVar = (q: V) => Variable(s"z_${q}")
  val stateUseCountVar = (q: V) => Variable(s"n_${q}")
  val isStartVar = (q: V) => Variable(s"start_$q")
  val isFinVar = (q: V) => Variable(s"fin_$q")
  private val SumYdTargetToQ = (q: V) => Variable(s"sum_y_target_${q}")
  private val SumYdSourceFrQ = (q: V) => Variable(s"sum_y_source_${q}")

  class MPGraphVars(implicit ctx: MPContext) {
    val edgeUseCountMinus05Vars = (e: EdgeId) =>
      if (ctx.integerEnabled)
        edgeUseCountVars(e)
      else {
        ctx.vars.getOrElseUpdate(EdgeUseCountVar(e).name + "-0.5", op = {
          val newVar = ctx.solver.makeNumVar(-0.5, MPInf, EdgeUseCountVar(e).name)
          val cons = ctx.solver.makeConstraint(0.5, 0.5)
          cons.setCoefficient(edgeUseCountVars(e), 1)
          cons.setCoefficient(newVar, -1)

          newVar
        })
      }
    val edgeUseCountVars = (e: EdgeId) =>
      if(ctx.integerEnabled)
        ctx.vars.getOrElseUpdate(EdgeUseCountVar(e).name, ctx.solver.makeIntVar(0, MPInf, EdgeUseCountVar(e).name))
      else {
        ctx.vars.getOrElseUpdate(EdgeUseCountVar(e).name, op = {
          assert(!ctx.vars.contains(e))
          val newVar = ctx.solver.makeNumVar(0, MPInf, EdgeUseCountVar(e).name)
//          val distVar = ctx.solver.makeNumVar(0, MPInf, EdgeUseCountVar(e).name)
//          val relaxedIntCons1 = ctx.solver.makeConstraint(MPNegInf,0.5)
//          relaxedIntCons1.setCoefficient(newVar, 1)
//          relaxedIntCons1.setCoefficient(distVar, -1)
//
//          val relaxedIntCons2 = ctx.solver.makeConstraint( 0.5, MPInf)
//          relaxedIntCons2.setCoefficient(newVar, 1)
//          relaxedIntCons2.setCoefficient(distVar, 1)
//
//          ctx.solver.objective().setCoefficient(distVar, 1)
          newVar
        })
      }
  }

  def flowConstraintInLP(source: V, sink: V)(implicit ctx: MPContext) = {
    val solver = ctx.solver
    val vars = MPGraphVars()

    states.foreach(s => {
      val sum = (if(s == sink) 1 else 0) - (if(s == source) 1 else 0)
      val cons = solver.makeConstraint(sum, sum)

      sourceFrom(s).foreach(going =>
        cons.setCoefficient(vars.edgeUseCountVars(going.id), -1)
      )
      targetTo(s).foreach(coming =>
        cons.setCoefficient(vars.edgeUseCountVars(coming.id), 1)
      )
    })
  }


  def connectivityConstraintInLPWithIndegreeEQ(start: V, edgeUseCount: Map[EdgeId, Int])(implicit ctx: MPContext) = {
    val vars = MPGraphVars()

    val uf = getConnectedComponent(edgeUseCount)
    val startComponent = uf.find(start)

    val components = uf.findRoots.diff(Set(startComponent))

    println(components.size)
    println(states.filter(s => uf.find(s) == components.head).toSeq.map(s => s.hashCode()).sorted)

    components.foreach(target => {
      val cons = ctx.solver.makeConstraint(0, MPInf)

      val vs = states.filter(s => uf.find(s) == target)
      val innerEdges = edgeUseCount.filter((eid, count) =>  vs.contains(idToedgesMap(eid).from) && vs.contains(idToedgesMap(eid).to))
      val incomings = vs.flatMap(targetTo).filter(e => !vs.contains(e.from))

      println(innerEdges.size)
      println(incomings.size)

      innerEdges.keys.foreach(v => {
        cons.setCoefficient(vars.edgeUseCountVars(v), -1)
      })
      incomings.foreach(v => {
        cons.setCoefficient(vars.edgeUseCountVars(v.id), 10000)
      })
    })
  }

  def connectivityConstraintInLP(start: V, edgeUseCount: Map[EdgeId, Int])(implicit ctx: MPContext) = {

    val vars = MPGraphVars()

    val uf = getConnectedComponent(edgeUseCount)
    val startComponent = uf.find(start)

    val components = uf.findRoots.diff(Set(startComponent))

    val vertexIdx: Map[V, Int] = states.toSeq.zipWithIndex.toMap

    val mf = MaxFlow()

    edges.foreach(t => {
      val fc = uf.find(t.from)
      val tc = uf.find(t.to)
      mf.addArcWithCapacity(vertexIdx(t.from), vertexIdx(t.to),
        if (fc == tc) Long.MaxValue / 10
        else 1
      )
    })
    val edgeIdToIdx: Map[EdgeId, Int] = edges.zipWithIndex.map((e,idx) => (e.id, idx)).toMap

    println(components.size)
    println(states.filter(s=>uf.find(s) == components.head).toSeq.map(s=>s.hashCode()).sorted)

    components.foreach(target => {
      assert(mf.solve(vertexIdx(start), vertexIdx(target)) == MaxFlow.Status.OPTIMAL)
//      val flowE = edges.zipWithIndex.map((e, idx) => (e.id, Math.min(30, mf.getCapacity(idx) - mf.getFlow(idx)).toInt)).toMap
//      saveSVG("flowE", "", flowE)

      val cut = {
        val reachable = mutable.HashSet[V]()
        val que = mutable.Queue[V](start)
        while (que.nonEmpty) {
          val s = que.dequeue()
          if (!reachable.contains(s)) {
            reachable.add(s)
            que.addAll(sourceFrom(s).filter(e => mf.getFlow(edgeIdToIdx(e.id)) < mf.getCapacity(edgeIdToIdx(e.id))).filter(e => !reachable.contains(e.to)).map(_.to))
            que.addAll(targetTo(s).filter(e => mf.getFlow(edgeIdToIdx(e.id)) > 0).filter(e => !reachable.contains(e.from)).map(_.from))
          }
        }

        edges.filter(e => {
          mf.getCapacity(edgeIdToIdx(e.id)) > 0 && reachable.contains(e.from) && !reachable.contains(e.to)
        })
      }
      assert(cut.size == mf.getOptimalFlow)

      val cons = ctx.solver.makeConstraint(0, MPInf)
      cut.foreach(e =>
        cons.setCoefficient(vars.edgeUseCountVars(e.id), 10000)
      )

      val vs = states.filter(s => uf.find(s) == target)
      val innerEdges = edgeUseCount.filter((eid, count) => vs.contains(idToedgesMap(eid).from) && vs.contains(idToedgesMap(eid).to))
      innerEdges.keys.foreach(wa =>
        cons.setCoefficient(vars.edgeUseCountVars(wa), -1)
      )
    })
  }


  def pathConstraint: ExistentialPresburgerFormula = {
    val notReached = (q: V) => And(
      Equal(
        stateUseCountVar(q),
        Constant(0)
      ),
      Equal(
        distanceVar(q),
        Constant(-1)
      )
    )
    val isReached = (q: V) =>
      And(
        GreaterThan(
          stateUseCountVar(q),
          Constant(0)
        ),
        Or(
          And(
            Equal(
              distanceVar(q),
              Constant(0)
            ),
            Equal(
              isStartVar(q),
              Constant(1),
            )
          ),
          OrList(targetTo(q).map(t =>
            AndList(List(
              Equal(
                distanceVar(q),
                Add(
                  distanceVar(t.from),
                  Constant(1)
                )
              ),
              GreaterThan(
                EdgeUseCountVar(t.id),
                Constant(0)
              ),
              GreaterThanOrEqual(
                distanceVar(t.from),
                Constant(0)
              )
            ))
          ))
        )
      )

    val onlyOneFinCons = And(
      Equal(
        states.map(state => isFinVar(state)).foldLeft[PresburgerExpression](Constant(0))((l, r) => Add(l, r)),
        Constant(1),
      ),
      AndList(
        states.map(state => GreaterThanOrEqual(isFinVar(state), Constant(0))).toSeq
      )
    )

    val onlyOneStCons = And(
      Equal(
        states.map(state => isStartVar(state)).foldLeft[PresburgerExpression](Constant(0))((l, r) => Add(l, r)),
        Constant(1),
      ),
      AndList(
        states.map(state => GreaterThanOrEqual(isStartVar(state), Constant(0))).toSeq
      )
    )

    val formulas: ListBuffer[Option[ExistentialPresburgerFormula]] = ListBuffer()
    formulas += Some(onlyOneFinCons)
    formulas += Some(onlyOneStCons)
    formulas ++= edges.map(t =>
      Some(GreaterThanOrEqual(EdgeUseCountVar(t.id), Constant(0)))
    )
    formulas ++= states.map(q =>
      Some(Equal(
        targetTo(q).map(t=>EdgeUseCountVar(t.id)).foldLeft[PresburgerExpression](Constant(0))((l, r) => Add(l, r)),
        SumYdTargetToQ(q)
      ))
    )
    formulas ++= states.map(q =>
      Some(Equal(
        sourceFrom(q).map(t=>EdgeUseCountVar(t.id))
          .foldLeft[PresburgerExpression](Constant(0))((l, r) => Add(l, r)),
        SumYdSourceFrQ(q)
      ))
    )
    formulas ++= states.map(q =>
      Some(Equal(
        Add(
          Sub(
            SumYdTargetToQ(q),
            SumYdSourceFrQ(q)
          ),
          Sub(
            isStartVar(q),
            isFinVar(q)
          )
        ),
        Constant(0)
      ))
    )
    formulas ++= states.map(q =>
      Some(Equal(
        stateUseCountVar(q),
        Add(
          SumYdTargetToQ(q),
          isStartVar(q)
        )
      ))
    )
    formulas ++= states.map(q =>
      Some(Equal(
        stateUseCountVar(q),
        Add(
          SumYdSourceFrQ(q),
          isFinVar(q)
        )
      ))
    )
    formulas ++= states.map(
      q => Some(Or(isReached(q), notReached(q)))
    )

    formulas.flatten.reduce(And.apply)
  }

  def solveEdgeUseCount(constraint: ExistentialPresburgerFormula): Option[Map[EdgeId, Int]] = {
      PresburgerFormulaSolver().solveWithZ3(constraint) match {
        case Some(m) =>
//          println(m.prettyPrint)
          Some(edges.map(trans =>
            (trans.id, m(EdgeUseCountVar(trans.id).name))
          ).toMap)
        case None =>
//          println(s"UNSAT core: ${PresburgerFormulaSolver().findUnSatCore(constraint).get.enumerateVar}")
          None
    }
  }

  def findReachables(from: V): Set[V] = {
    val ret = MutableSet[V]()

    def f(cur: V): Unit = {
      if(!ret.contains(cur)) {
        ret.add(cur)
        sourceFrom(cur).foreach{next => f(next.to)}
      }
    }

    f(from)
    ret.toSet
  }

  def printDot(name: String = "", useCountMap:Map[EdgeId, Int] = Map(), additional:String=""): String =
    s"digraph $name {\n" + additional + "\n" + (
      if(useCountMap.isEmpty)
         edges.map { e => s"\"${e.from}\" -> \"${e.to}\" [label=\"${e}\", style = solid ];" }.mkString("\n")
      else
//        edges.map { e => s"\"${e.from}\" -> \"${e.to}\" [label=\"${e}\n${useCountMap.getOrElse(e.id, 0)}\", style = ${if (useCountMap.getOrElse(e.id, 0) > 0) "solid" else "dotted"}];" }.mkString("\n")
          edges.filter(e => useCountMap.getOrElse(e.id, 0) > 0).map { e => s"\"${e.from}\" -> \"${e.to}\" [label=\"${e}\n${useCountMap.getOrElse(e.id, 0)}\", style = ${if (useCountMap.getOrElse(e.id, 0) > 0) "solid" else "dotted"}];" }.mkString("\n")
    ) + "}"

  def saveDot(fileName: String, name: String = "", useCountMap:Map[EdgeId, Int] = Map()): Unit = {
    Files.write(Paths.get(fileName), printDot(name, useCountMap).getBytes(StandardCharsets.UTF_8))
  }

  def saveSVG(baseName: String, name: String="", useCountMap:Map[EdgeId, Int] = Map()): Unit = {
    saveDot(baseName + ".dot", name, useCountMap)
    Files.write(
      Paths.get(baseName + ".svg"),
      Process(s"dot -Tsvg ${baseName + ".dot"}").!!.getBytes(StandardCharsets.UTF_8)
    )
  }

  def getConnectedComponent(edgeUseCount: Map[EdgeId, Int]): UnionFind[V] = {
    val uf = UnionFind[V]()

    edgeUseCount.map((id, count) => {
      if (count > 0) {
        uf.union(idToedgesMap(id).from, idToedgesMap(id).to)
      }
    })

    uf
  }
  def isConnected(edgeUseCount: Map[EdgeId, Int]): Boolean = {
    getConnectedComponent(edgeUseCount).findRoots.size == 1
  }

}

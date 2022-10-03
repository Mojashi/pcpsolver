package graph

import presburger.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Set as MutableSet, Map as MutableMap}
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
    edges.filter(t => t.to == q)
  }

  val distanceVar = (q: V) => Variable(s"z_${q}")
  val stateUseCountVar = (q: V) => Variable(s"n_${q}")
  val isStartVar = (q: V) => Variable(s"start_$q")
  val isFinVar = (q: V) => Variable(s"fin_$q")
  private val SumYdTargetToQ = (q: V) => Variable(s"sum_y_target_${q}")
  private val SumYdSourceFrQ = (q: V) => Variable(s"sum_y_source_${q}")

  def edgeUseCountIsPositiveConstraint = {

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
      PresburgerFormulaSolver().solve(constraint) match {
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

  def printDot(name: String = "", useCountMap:Map[EdgeId, Int] = Map()): String =
    s"""digraph $name {
       ${edges.map { e => s"\"${e.from}\" -> \"${e.to}\" [label=\"${e}\", style = ${if(useCountMap.isEmpty || useCountMap.getOrElse(e.id, 0) > 0) "solid" else "dotted" }];" }.mkString("\n")}
       }"""

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
}

implicit class PrettyPrintMap[K, V](val map: Map[K, V]) {
  def prettyPrint: PrettyPrintMap[K, V] = this

  override def toString: String = {
    val valuesString = toStringLines.mkString("\n")

    "Map (\n" + valuesString + "\n)"
  }

  def toStringLines = {
    map
      .flatMap{ case (k, v) => keyValueToString(k, v)}
      .map(indentLine(_))
  }

  def keyValueToString(key: K, value: V): Iterable[String] = {
    value match {
      case v: Map[_, _] => Iterable(key.toString + " -> Map (") ++ v.prettyPrint.toStringLines ++ Iterable(")")
      case x => Iterable(key.toString + " -> " + x.toString)
    }
  }

  def indentLine(line: String): String = {
    "\t" + line
  }

}
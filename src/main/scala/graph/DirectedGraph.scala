package graph

import presburger.*

import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map as MutableMap}

type EdgeId = String
trait EdgeLike[V] {
  val from: V
  val to: V
  val id: EdgeId
}

case class Edge[V](from: V, to: V, id: EdgeId) extends EdgeLike[V]

class DirectedGraph[V, E <: EdgeLike[V]]
(
  edges: Seq[E]
) {
  val states: Set[V] = edges.flatMap(t => Set(t.from, t.to)).toSet

  val edgesMap: Map[V, Seq[E]] = {
    edges.groupBy(t => t.from)
  }
  val idToedgesMap: Map[EdgeId, E] =
    edges.map(t => (t.id, t)).toMap
    
  def eulerTrail(start: V, useCount: Map[EdgeId, Int]): Seq[E] = {
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

      trail.reverse.toSeq
  }

  def extractEdgeUseCount(m: VarValueMap): Map[EdgeId, Int] =
    edges.map(trans =>
      (trans.id, m(transOccurCountVar(trans).name))
    ).toMap

  def sourceFrom(q: V): Seq[E] = {
    edgesMap.getOrElse(q, Seq())
  }

  def targetTo(q: V): Seq[E] = {
    edges.filter(t => t.to == q)
  }

  val transOccurCountVar = (t: E) => Variable(s"y_${t.id}")
  val distanceVar = (q: V) => Variable(s"z_${q}")
  val stateOccurCountVar = (q: V) => Variable(s"n_${q}")
  val isStartVar = (q: V) => Variable(s"start_$q")
  val isFinVar = (q: V) => Variable(s"fin_$q")
  val SumYdTargetToQ = (q: V) => Variable(s"sum_y_target_${q}")
  val SumYdSourceFrQ = (q: V) => Variable(s"sum_y_source_${q}")

  def pathConstraint: ExistentialPresburgerFormula = {
    val notReached = (q: V) => And(
      Equal(
        stateOccurCountVar(q),
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
          stateOccurCountVar(q),
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
                transOccurCountVar(t),
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
        states.map(state => isFinVar(state)).reduce((l, r) => Add(l, r)),
        Constant(1),
      ),
      AndList(
        states.map(state => GreaterThanOrEqual(isFinVar(state), Constant(0))).toSeq
      )
    )

    val onlyOneStCons = And(
      Equal(
        states.map(state => isStartVar(state)).reduce((l, r) => Add(l, r)),
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
      Some(GreaterThanOrEqual(transOccurCountVar(t), Constant(0)))
    )
    formulas ++= states.map(q =>
      Some(Equal(
        targetTo(q).map(transOccurCountVar).foldLeft[PresburgerExpression](Constant(0))((l, r) => Add(l, r)),
        SumYdTargetToQ(q)
      ))
    )
    formulas ++= states.map(q =>
      Some(Equal(
        sourceFrom(q).map(transOccurCountVar)
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
        stateOccurCountVar(q),
        Add(
          SumYdTargetToQ(q),
          isStartVar(q)
        )
      ))
    )
    formulas ++= states.map(q =>
      Some(Equal(
        stateOccurCountVar(q),
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
}

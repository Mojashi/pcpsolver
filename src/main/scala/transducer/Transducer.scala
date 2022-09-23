package transducer

import graph.{DirectedGraph, Edge, EdgeId, EdgeLike}

import scala.:+

type State = String

case class Transition[State, Label]
  (from: State, to: State, in: String, out: Label, id: EdgeId)
  extends EdgeLike[State]

trait Monoid[A] {
  def plus(l: A, r: A): A
  val unit: A
}

class Transducer[OutMonoid: Monoid]
(
  val start: State,
  val fin: Set[State],
  val transitions: Seq[Transition[State, OutMonoid]],
) extends DirectedGraph[State, Transition[State, OutMonoid]](transitions) {
  private val outM = implicitly[Monoid[OutMonoid]]

  type InWord = String

  def transduce(word: InWord, from: State = start): Set[OutMonoid] = {
    if(word.isEmpty)
      if (fin.contains(from))
        Set(outM.unit)
      else Set()
    else
      edgesMap(from)
        .filter(trans => trans.in.startsWith(word))
        .flatMap(trans =>
          transduce(trans.in.substring(word.length), trans.to).map( rest =>
            outM.plus(trans.out, rest)
          )
        ).toSet
  }

  def expandTransition(id: EdgeId): Transducer[OutMonoid] = {
    val restTransitions = transitions.filter(t => t.id != id)
    val trans = idToedgesMap(id)
    val nexts = edgesMap(trans.to)

    val newFin = s"fin_${trans.id}"

    val finTransition = Transition(
      from = trans.from,
      to = newFin,
      in = trans.in,
      out = trans.out,
      id = s"tofin_${trans.id}"
    )

    val newTransitions = restTransitions ++ nexts.map(bTrans =>
      Transition(
        from = trans.from,
        to = bTrans.to,
        in = trans.in + bTrans.in,
        out = outM.plus(trans.out, bTrans.out),
        id = s"expand_($id)_(${bTrans.id})"
      )
    ) :+ finTransition

    Transducer(
      start = start,
      fin = fin + newFin,
      transitions = newTransitions
    )
  }
}

implicit object StringMonoid extends Monoid[String] {
  override val unit: String = ""

  override def plus(l: String, r: String): String = l ++ r
}

class ListMonoid[A] extends Monoid[List[A]] {
  override def plus(l: List[A], r: List[A]): List[A] = l++r

  override val unit: List[A] = List()
}
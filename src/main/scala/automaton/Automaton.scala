package automaton

import graph.{DirectedGraph, EdgeId, EdgeLike}
import presburger.{Add, AndList, Constant, Equal, ExistentialPresburgerFormula, Mul}

import scala.collection.mutable.ListBuffer

extension[Alphabet] (s: Seq[Alphabet]) {
  def consume(a: Option[Alphabet]): Seq[Alphabet] =
    a match
      case Some(c) =>
        if(s.head == c) s.tail
        else throw Error("cannot consume")
      case None => s

  def consumable(a: Option[Alphabet]): Boolean =
    a match
      case Some(c) => s.head == c
      case None => true
}

case class Transition[State, Alphabet]
(from: State, to: State, in: Alphabet, id: EdgeId)
  extends EdgeLike[State] {
  override def toString: _root_.java.lang.String = s"$in\n$id"
}

class EPSFreeNFA[State, Alphabet]
(
  start: State,
  fin: Set[State],
  transitions: Seq[Transition[State, Alphabet]],
) extends NFA[State, Alphabet] (
    start, fin, transitions.map(t=>Transition(t.from, t.to, Some(t.in), t.id))
)

class NFA[State, Alphabet]
(
  val start: State,
  val fin: Set[State],
  val transitions: Seq[Transition[State, Option[Alphabet]]],
) extends DirectedGraph[State, Transition[State, Option[Alphabet]]](transitions) {
  type T = Transition[State, Option[Alphabet]]

  def this(nfa: NFA[State,Alphabet]) = {
    this(nfa.start, nfa.fin,nfa.transitions)
  }

  def accept(word: Seq[Alphabet]): Boolean = {
    val reached = Set[(State, Seq[Alphabet])]()

    def f(word: Seq[Alphabet], from: State = start): Boolean = {
      if(reached.contains((from, word)))
        false
      else {
        if (word.isEmpty)
          fin.contains(from)
        else
          sourceFrom(from)
            .filter(trans => word.consumable(trans.in))
            .exists(trans => f(word.consume(trans.in), trans.to))
      }
    }

    f(word)
  }

  val alphabets = transitions.flatMap(t=>t.in).toSet

  def acceptConstraint: ExistentialPresburgerFormula = {
    val startCons = states.filter(s => s != start).map[ExistentialPresburgerFormula](s =>
      Equal(isStartVar(s), Constant(0))
    ).toSeq
    val finCons = states.diff(fin).map[ExistentialPresburgerFormula](s =>
      Equal(isFinVar(s), Constant(0))
    ).toSeq

    AndList(
      pathConstraint +: (startCons ++ finCons)
    )
  }

  def solveInputWord(constraint: ExistentialPresburgerFormula): Option[Seq[Alphabet]] =
    solveEdgeUseCount(constraint).flatMap( useCount =>
      eulerTrail(start, useCount).flatMap(trail =>
          Some(trail.map(t => t.in).flatten)
      )
    )
}

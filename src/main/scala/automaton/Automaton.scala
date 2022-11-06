package automaton

import dataType.{IntVector, IntVectorMonoid}
import graph.{DirectedGraph, EdgeId, EdgeLike, UniqueEdgeId}
import presburger.{Add, AndList, Constant, Equal, ExistentialPresburgerFormula, Mul}
import scala.collection.mutable.{ListBuffer, Map as MutableMap}

extension[Alphabet] (s: Seq[Alphabet]) {
  def consume(a: Option[Alphabet]): Seq[Alphabet] =
    a match
      case Some(c) =>
        if(s.head == c) s.tail
        else throw Error("cannot consume")
      case None => s

  def consumable(a: Option[Alphabet]): Boolean =
    a match
      case Some(c) => if(s.isEmpty) false else s.head == c
      case None => true
}

case class Transition[State, Alphabet]
(from: State, to: State, in: Alphabet, id: EdgeId)
  extends EdgeLike[State] {
  override def toString: String = in.toString
}

class OptionTransition[State, Alphabet]
(from: State, to: State, in: Option[Alphabet], id: EdgeId)
  extends Transition[State, Option[Alphabet]](from, to, in, id) {
  override def toString: String = if(in.isDefined) in.get.toString else "ε"
}

class EPSFreeNFA[State, Alphabet]
(
  start: State,
  fin: Set[State],
  val epsFreeTransitions: Seq[Transition[State, Alphabet]],
) extends NFA[State, Alphabet] (
    start, fin, epsFreeTransitions.map(t=>OptionTransition(t.from, t.to, Some(t.in), t.id))
) {
  def this(nfa: EPSFreeNFA[State, Alphabet]) = {
    this(nfa.start, nfa.fin, nfa.epsFreeTransitions)
  }
}

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

  override def printDot[T:Numeric](name: String = "", useCountMap: Map[EdgeId, T] = Map(), additional:String = ""): String =
    super.printDot(name, useCountMap, additional + "\nsuperstart[shape = point ];\n" + s"superstart->\"$start\"\n" + fin.map(f => s"\"$f\" [shape=doublecircle];").mkString("\n"))

  def setSingleFin: NFA[Either[State, String], Alphabet] = {
    val newFin = Right("newFin")
    NFA(
      start = Left(start),
      fin = Set(newFin),
      transitions = transitions.map(t =>
        Transition(
          from = Left(t.from),
          to = Left(t.to),
          in = t.in,
          id = t.id
        )
      ) ++ fin.map(f =>
        Transition(
          from = Left(f),
          to = newFin,
          in = None,
          id = s"newFin($f)"
        )
      )
    )
  }

  def setNewStartFinState: NFA[Either[State, String], Alphabet] = {
    val newFin = Right("newFin")
    val newStart = Right("newStart")
    NFA(
      start = newStart,
      fin = Set(newFin),
      transitions = transitions.map(t=>
        Transition(
          from = Left(t.from),
          to = Left(t.to),
          in = t.in,
          id = t.id
        )
      ) ++ fin.map(f =>
        Transition(
          from = Left(f),
          to = newFin,
          in = None,
          id = s"newFin($f)"
        )
      ) ++ Seq(Transition(
        from = newStart,
        to = Left(start),
        in = None,
        id = s"newStart(${start})"
      ))
    )
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

  def truncateUnReachable: NFA[State, Alphabet] = {
    val vertices = findReachables(start)
    NFA(
      start = start,
      fin = fin.intersect(vertices),
      transitions = transitions.filter(t => vertices.contains(t.from))
    )
  }

  def parikhAutomaton: ParikhAutomaton[State, Alphabet] = {
    val m = IntVectorMonoid[Alphabet]()
    ParikhAutomaton[State, Alphabet](
      start, fin, transitions.map(t => Transition(
        t.from, t.to, t.in match {
          case Some(c) => Map((c , 1))
          case None => Map()
        }, t.id
      ))
    )(m)
  }

  def getUseCount(word: Seq[Alphabet]): Map[EdgeId, Int] = {
    val reached = Set[(State, Seq[Alphabet])]()
    val useCount = MutableMap[EdgeId, Int]()

    def f(word: Seq[Alphabet], from: State = start): Boolean = {
      if (reached.contains((from, word)))
        false
      else {
        if (word.isEmpty && fin.contains(from))
          true
        else {
          val trans = sourceFrom(from)
            .filter(trans => word.consumable(trans.in))
            .find(trans => f(word.consume(trans.in), trans.to))
          trans match
            case Some(t) => useCount(t.id) = useCount.getOrElse(t.id, 0) + 1
            case None => return false

          trans.isDefined
        }
      }
    }

    f(word)
    useCount.toMap
  }

  def stateAny() =
    NFA[Any, Alphabet](
      start, fin.toSet, transitions.map(e => Transition(e.from, e.to, e.in, e.id))
    )
    
  def uniquify = NFA(
    start, fin,
    transitions.map(t => Transition(t.from, t.to, t.in, UniqueEdgeId.get))
  )
}

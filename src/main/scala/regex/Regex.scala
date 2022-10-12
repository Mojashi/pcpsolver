package regex

import automaton.{EPSFreeNFA, NFA, Transition}
import dataType.Monoid
import graph.UniqueEdgeId
import transducer.NormalFormTransducer

import scala.collection.mutable.{ListBuffer, Map as MutableMap}

trait Regex[Alphabet] {
  def size: BigInt
}
case class Or[Alphabet](r1: Regex[Alphabet], r2: Regex[Alphabet]) extends Regex[Alphabet] {
  override def toString = s"($r1)|($r2)"
  override val size = r1.size + r2.size + 1
}
case class Constant[Alphabet](s: Alphabet) extends Regex[Alphabet]{
  override def toString = s"$s"
  override val size = 0
}
case class EPS[Alphabet]() extends Regex[Alphabet]{
  override def toString = s""
  override val size = 0
}
case class Cons[Alphabet](r1: Regex[Alphabet], r2: Regex[Alphabet]) extends Regex[Alphabet]{
  override def toString = s"$r1$r2"
  override val size = r1.size + r2.size
}
case class Star[Alphabet](r: Regex[Alphabet]) extends Regex[Alphabet]{
  override def toString = s"($r)*"
  override val size = r.size
}

extension[State, Alphabet] (nfa: NFA[State, Alphabet]) {
  def liftToRegexNFA: EPSFreeNFA[Any, Regex[Alphabet]] = {
    val normalized = nfa.setNewStartFinState.truncateUnReachable.stateAny()
    val start = normalized.start
    val fin = normalized.fin.head

    val transitions = normalized.transitions.map (t =>
      Transition[Any, Regex[Alphabet]] (
        t.from, t.to, t.in match
        case Some (ch) => Constant (ch)
        case None => EPS ()
        , t.id
      )
    )

    EPSFreeNFA(
      start = normalized.start,
      fin = normalized.fin.map(identity),
      epsFreeTransitions = transitions
    )
  }

  def simplifyWithOutIncrease: EPSFreeNFA[Any, Regex[Alphabet]] = {
    val regexNFA = nfa.liftToRegexNFA
    var transitions = regexNFA.epsFreeTransitions
    val start = regexNFA.start
    val fin = regexNFA.fin

    while (true) {
      val fromMap = MutableMap[Any, ListBuffer[Transition[Any, Regex[Alphabet]]]]()
      val toMap = MutableMap[Any, ListBuffer[Transition[Any, Regex[Alphabet]]]]()

      transitions.foreach(t => {
        fromMap.getOrElseUpdate(t.from, ListBuffer()).addOne(t)
        toMap.getOrElseUpdate(t.to, ListBuffer()).addOne(t)
      })

      val targetOpt = fromMap.keys.find(state => {
        (fromMap.get(state), toMap.get(state)) match{
          case (Some(t1), Some(t2)) => t1.size * t2.size <= t1.size + t2.size
          case _=>false
        }
      })

      targetOpt match
        case None =>
          return EPSFreeNFA(
            start = start,
            fin = fin,
            epsFreeTransitions = transitions
          )
        case Some(target) => {
          val nexts = transitions.filter(t => t.from == target && t.to != target)
          val prevs = transitions.filter(t => t.to == target && t.from != target)

          val loops = transitions.filter(t => t.to == target && t.from == target).map(l => Some(l)) :+ None

          val newTransitions =
            nexts.flatMap[Transition[Any, Regex[Alphabet]]](going =>
              loops.flatMap(loop =>
                prevs.map(coming =>
                  Transition(
                    from = coming.from,
                    to = going.to,
                    in = loop match {
                      case Some(t) => Cons(Cons(coming.in, Star(t.in)), going.in)
                      case None => Cons(coming.in, going.in)
                    },
                    id = UniqueEdgeId.get
                  )
                )
              )
            )

          transitions = (transitions.filter(t => t.from != target && t.to != target) ++ newTransitions)
        }
    }

    throw Error("unreachable")
  }



def toRegex: Regex[Alphabet] = {
    val regexNFA = nfa.liftToRegexNFA
    var transitions = regexNFA.epsFreeTransitions.omitDouble
    val start = regexNFA.start
    val fin = regexNFA.fin.head

    while(transitions.size > 1) {
      val targetT = transitions.find(t => t.from != start || t.to != fin).get
      val target = if(targetT.from != start) targetT.from else targetT.to

      val nexts = transitions.filter(t => t.from == target && t.to != target)
      val prevs = transitions.filter(t => t.to == target && t.from != target)

      val loop = transitions.find(t => t.to == target && t.from == target)

      val newTransitions = nexts.flatMap[Transition[Any, Regex[Alphabet]]](going =>
        prevs.map(coming =>
          Transition(
            from = coming.from,
            to = going.to,
            in = loop match {
              case Some(t) => Cons(Cons(coming.in, Star(t.in)), going.in)
              case None => Cons(coming.in, going.in)
            },
            id = UniqueEdgeId.get
          )
        )
      )

      transitions = (transitions.filter(t => t.from != target && t.to != target) ++ newTransitions).omitDouble
    }

    transitions.head.in
  }
}

extension[State, Alphabet] (transitions: Seq[Transition[State, Regex[Alphabet]]]) {
  def omitDouble: Seq[Transition[State, Regex[Alphabet]]] = {
    transitions
      .groupBy(t => (t.from, t.to))
      .map { case (key, t) =>
          Transition(
            from = t.head.from,
            to = t.head.to,
            in = t.map(t => t.in).reduce((l, r) => Or(l, r)),
            id = UniqueEdgeId.get,
          )
      }.toSeq
  }
}
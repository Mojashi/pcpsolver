package automaton

import dataType.*
import graph.{EdgeId, EdgeUseCountVar}
import presburger.*
import transducer.Transducer

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
  val KeyCountVar: (Key=>Variable) = (key: Key) => Variable(prefixForKeyCount + key)

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
}

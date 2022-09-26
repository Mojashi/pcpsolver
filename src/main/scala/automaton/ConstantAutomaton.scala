package automaton

import graph.UniqueEdgeId

class ConstantAutomaton[Alphabet]
(
  val name: String,
  val s: Seq[Alphabet]
) extends EPSFreeNFA[Int, Alphabet](
  start = 0,
  fin = Set(s.length),
  epsFreeTransitions = s.zipWithIndex.map((ch, idx) =>
    Transition(
      from = idx,
      to = idx + 1,
      in = ch,
      id = s"const($name,$idx)"
    )
  )
)
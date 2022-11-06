package solver

import automaton.{NFA, Transition}
import util.EdgeUseCountTracker
import graph.EdgeUseCountVar
import scala.collection.mutable.ListBuffer
class ParallelNFA[StateA, StateB, Alphabet]
(
  left: NFA[StateA, Alphabet],
  right: NFA[StateB, Alphabet]
)(
  implicit tracker: EdgeUseCountTracker = EdgeUseCountTracker()
) extends NFA[(StateA, StateB), Alphabet]({
  tracker.newSession()
  type NewTransition = Transition[(StateA, StateB), Option[Alphabet]]

  val alphabets = left.alphabets.intersect(right.alphabets).toSeq
  val newStates = left.states.flatMap(s1 => right.states.map(s2 => (s1, s2)))

  val nt = ListBuffer[NewTransition]()

  nt ++= left.transitions.filter(t1 => t1.in.isDefined).flatMap(t1 =>
    right.transitions.filter(t2 => t2.in == t1.in).map(t2 => {
      val id = s"pr(${t1.id},${t2.id})"
      tracker.AddPart(t1.id, id)
      tracker.AddPart(t2.id, id)
      Transition(
        from = (t1.from, t2.from),
        to = (t1.to, t2.to),
        in = t1.in,
        id = id
      )
    })
  )

  nt ++= left.transitions.filter(t1 => t1.in.isEmpty).flatMap(t1 =>
    right.states.map(s => {
      val id = s"pr_none1${t1.id},${s})"
      tracker.AddPart(t1.id, id)
      Transition(
        from = (t1.from, s),
        to = (t1.to, s),
        in = None,
        id = s"pr_none1${t1.id},${s})"
      )
    })
  )

  nt ++= right.transitions.filter(t2 => t2.in.isEmpty).flatMap(t2 =>
    left.states.map(s => {
      val id = s"pr_none2${t2.id},${s})"
      tracker.AddPart(t2.id, id)
      Transition(
        from = (s, t2.from),
        to = (s, t2.to),
        in = None,
        id = s"pr_none2${t2.id},${s})"
      )
    })
  )

  NFA[(StateA, StateB), Alphabet](
    start = (left.start, right.start),
    fin = left.fin.flatMap(f1 => right.fin.map(f2 =>(f1, f2))),
    transitions = nt.toSeq
  ).truncateUnReachable
}) {

}


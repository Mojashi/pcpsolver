package automaton

import presburger.ExistentialPresburgerFormula

import scala.collection.mutable.ListBuffer
import presburger.*

class ParallelNFA[StateA, StateB, Alphabet]
(
  left: NFA[StateA, Alphabet],
  right: NFA[StateB, Alphabet]
) extends NFA[WaitingState[StateA, StateB, Alphabet], Either[Option[Alphabet], Option[Alphabet]]]({
  type NewAlphabet = Either[Option[Alphabet], Option[Alphabet]]
  type NewState = WaitingState[StateA, StateB, Alphabet]
  type NewTransition = Transition[WaitingState[StateA, StateB, Alphabet], Option[NewAlphabet]]

  val sumAlphabets = (left.alphabets ++ right.alphabets).toSeq
  val waitingState: Seq[NewState] = left.states.flatMap(s1 => right.states.flatMap(s2 => sumAlphabets.map(c => WaitingState(s1, s2, Some(c))))).toSeq
  val alignedState: Seq[NewState] = left.states.flatMap(s1 => right.states.map(s2 => WaitingState(s1, s2, None))).toSeq
  val newState = waitingState ++ alignedState

  val nt = ListBuffer[NewTransition]()
  nt ++= left.transitions.filter(t1 => t1.in.nonEmpty).flatMap(t1 =>
    alignedState.filter(state => state.left == t1.from).map(from =>
      Transition(
        from = from,
        to = WaitingState(t1.to, from.right, t1.in),
        in = Some(Left(t1.in)),
        id = s"${from}_${t1.id}"
      )
    )
  )

  nt ++= right.transitions.filter(t2 => t2.in.nonEmpty).flatMap(t2 =>
    waitingState.filter(state => state.right == t2.from && state.waiting == t2.in).map(from =>
      Transition(
        from = from,
        to = WaitingState(from.left, t2.to, None),
        in = Some(Right(t2.in)),
        id = s"${from}_${t2.id}"
      )
    )
  )

  nt ++= left.transitions.filter(t1 => t1.in.isEmpty).flatMap(t1 =>
    newState.filter(state => state.left == t1.from).map(from =>
      Transition(
        from = from,
        to = WaitingState(t1.to, from.right, from.waiting),
        in = Some(Left(t1.in)),
        id = s"${from}_${t1.id}"
      )
    )
  )

  nt ++= right.transitions.filter(t2 => t2.in.isEmpty).flatMap(t2 =>
    newState.filter(state => state.right == t2.from).map(from =>
      Transition(
        from = from,
        to = WaitingState(from.left, t2.to, from.waiting),
        in = Some(Right(t2.in)),
        id = s"${from}_${t2.id}"
      )
    )
  )

  NFA[WaitingState[StateA, StateB, Alphabet], NewAlphabet](
    start = WaitingState(left.start, right.start, None),
    fin = left.fin.flatMap(f1 => right.fin.map(f2 => WaitingState(f1, f2, None))),
    transitions = nt.toSeq
  )
}) {

  def acceptConstraint: ExistentialPresburgerFormula = {
    val formulas = ListBuffer(pathConstraint)
    formulas ++= states.filter(s => s != start).map(s =>
      Equal(isStartVar(s), Constant(0))
    )
    formulas ++= states.diff(fin).map(s =>
      Equal(isFinVar(s), Constant(0))
    )
    AndList(formulas.toSeq)
  }

  def solve = {
    PresburgerFormulaSolver()
      .solve(acceptConstraint)
      .flatMap(m => {
        println(m)
        Some(eulerTrail(start, extractEdgeUseCount(m)).map(t => t.in))
      })
  }
}

case class WaitingState[StateA, StateB, Alphabet](left:StateA, right: StateB, waiting: Option[Alphabet])

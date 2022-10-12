package transducer

import automaton.{NFA, Transition}
import dataType.*
import graph.{EdgeId, EdgeLike, EdgeUseCountVar}
import util.EdgeUseCountTracker

import scala.collection.mutable.ListBuffer

case class NormalFormTransducerTransition[State, InAlphabet, OutAlphabet]
(from: State, to: State, in: Option[InAlphabet], out: Option[OutAlphabet], id: EdgeId)
  extends EdgeLike[State] {
  override def toString: _root_.java.lang.String = s"$in / $out\n$id"
}

class NormalFormTransducer[State, InAlphabet, OutAlphabet]
(
  start: State,
  fin: Set[State],
  val normalTransitions: Seq[NormalFormTransducerTransition[State, InAlphabet, OutAlphabet]]
) extends Transducer[State, InAlphabet, List[OutAlphabet]](
  start, fin, normalTransitions.map(t=>TransducerTransition(t.from, t.to, t.in,
    t.out match {
      case Some(i) => List(i)
      case None => List()
    }, t.id))
)(ListMonoid[OutAlphabet]()) {

  def combine[StateB]
  (
    other: NFA[StateB, OutAlphabet],
  )(implicit tracker: EdgeUseCountTracker): NFA[(State, StateB), InAlphabet] = {
    tracker.newSession()

      val newTransitions = ListBuffer[Transition[(State, StateB), Option[InAlphabet]]]()

      newTransitions ++= normalTransitions.flatMap(t1 =>
        other.transitions
          .filter(t2 => t2.in == t1.out && t2.in.isDefined)
          .map[Transition[(State, StateB), Option[InAlphabet]]](t2 => {
            val id = s"(${t1.id},${t2.id})"
            tracker.AddPart(EdgeUseCountVar(t1.id), EdgeUseCountVar(id))
            tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))

            Transition(
              from = (t1.from, t2.from),
              to = (t1.to, t2.to),
              in = t1.in,
              id = id
            )
          })
      )

      newTransitions ++= normalTransitions.filter(t1 => t1.out.isEmpty).flatMap(t1 =>
        other.states.map(s2 => {
          val id = s"(${t1.id},from${s2})"
          tracker.AddPart(EdgeUseCountVar(t1.id), EdgeUseCountVar(id))

          Transition(
            from = (t1.from, s2),
            to = (t1.to, s2),
            in = t1.in,
            id = id
          )
        })
      )

      newTransitions ++= other.transitions.filter(t2 => t2.in.isEmpty).flatMap(t2 =>
        states.map(s1 => {
          val id = s"(from$s1,${t2.id})"
          tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))

          Transition(
            from = (s1, t2.from),
            to = (s1, t2.to),
            in = None,
            id = id
          )
        })
      )

      NFA(
        start = (start, other.start),
        fin = fin.flatMap(f1 => other.fin.map(f2 => (f1, f2))),
        transitions = newTransitions.toSeq
      ).truncateUnReachable
  }
  def combine[StateB, OutAlphabetB]
  (
    other: NormalFormTransducer[StateB, OutAlphabet, OutAlphabetB],
  )(implicit tracker: EdgeUseCountTracker = EdgeUseCountTracker()):
    NormalFormTransducer[(State, StateB), InAlphabet, OutAlphabetB] = {
    tracker.newSession()

    val newTransitions = ListBuffer[NormalFormTransducerTransition[(State, StateB), InAlphabet, OutAlphabetB]]()

    newTransitions ++= normalTransitions.flatMap(t1 =>
      other.normalTransitions
        .filter(t2=>t2.in==t1.out&&t2.in.isDefined)
        .map[NormalFormTransducerTransition[(State, StateB), InAlphabet, OutAlphabetB]](t2 => {
          val id = s"(${t1.id},${t2.id})"
          tracker.AddPart(EdgeUseCountVar(t1.id), EdgeUseCountVar(id))
          tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))

          NormalFormTransducerTransition(
            from = (t1.from, t2.from),
            to = (t1.to, t2.to),
            in = t1.in,
            out = t2.out,
            id = id
          )
      })
    )

    newTransitions ++= normalTransitions.filter(t1 => t1.out.isEmpty).flatMap(t1 =>
      other.states.map(s2 => {
        val id = s"(${t1.id},from${s2})"
        tracker.AddPart(EdgeUseCountVar(t1.id), EdgeUseCountVar(id))

        NormalFormTransducerTransition(
          from = (t1.from, s2),
          to = (t1.to, s2),
          in = t1.in,
          out = None,
          id = id
        )
      })
    )

    newTransitions ++= other.normalTransitions.filter(t2 => t2.in.isEmpty).flatMap(t2 =>
      states.map(s1 => {
        val id = s"(from$s1,${t2.id})"
        tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))

        NormalFormTransducerTransition(
          from = (s1, t2.from),
          to = (s1, t2.to),
          in = None,
          out = t2.out,
          id = id
        )
      })
    )

    NormalFormTransducer(
      start = (start, other.start),
      fin = fin.flatMap(f1=>other.fin.map(f2=>(f1,f2))),
      normalTransitions = newTransitions.toSeq
    ).truncateUnReachable
  }

  def truncateUnReachable: NormalFormTransducer[State, InAlphabet, OutAlphabet] = {
    val vertices = findReachables(start)
    NormalFormTransducer(
      start = start,
      fin = fin.intersect(vertices),
      normalTransitions = normalTransitions.filter(t=>vertices.contains(t.from))
    )
  }


  override def addPrefix(prefix: String):NormalFormTransducer[(String, State), InAlphabet, OutAlphabet] = NormalFormTransducer[(String, State), InAlphabet, OutAlphabet](
    start = (prefix, start),
    fin = fin.map(f => (prefix,f)),
    normalTransitions = normalTransitions.map(t =>
      NormalFormTransducerTransition(
        from = (prefix,t.from),
        to = (prefix,t.to),
        in = t.in,
        out = t.out,
        id = s"${prefix}_${t.id}",
      )
    )
  )

  def product[StateB, OutAlphabetB](right: NormalFormTransducer[StateB, InAlphabet, OutAlphabetB])(
    implicit tracker: EdgeUseCountTracker = EdgeUseCountTracker()
  ) = {
    tracker.newSession()
    type NewTransition = NormalFormTransducerTransition[(State, StateB), InAlphabet, (Option[OutAlphabet], Option[OutAlphabetB])]

    val nt = ListBuffer[NewTransition]()

    nt ++= normalTransitions.filter(t1 => t1.in.isDefined).flatMap(t1 =>
      right.normalTransitions.filter(t2 => t2.in == t1.in).map(t2 => {
        val id = s"pr(${t1.id},${t2.id})"
        tracker.AddPart(EdgeUseCountVar(t1.id), EdgeUseCountVar(id))
        tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))
        NormalFormTransducerTransition(
          from = (t1.from, t2.from),
          to = (t1.to, t2.to),
          in = t1.in,
          out = Some((t1.out, t2.out)),
          id = id
        )
      })
    )

    nt ++= normalTransitions.filter(t1 => t1.in.isEmpty).flatMap(t1 =>
      right.states.map(s => {
        val id = s"pr_none1${t1.id},${s})"
        tracker.AddPart(EdgeUseCountVar(t1.id), EdgeUseCountVar(id))
        NormalFormTransducerTransition(
          from = (t1.from, s),
          to = (t1.to, s),
          in = None,
          out = Some((t1.out, None)),
          id = id
        )
      })
    )

    nt ++= right.normalTransitions.filter(t2 => t2.in.isEmpty).flatMap(t2 =>
      states.map(s => {
        val id = s"pr_none2${t2.id},${s})"
        tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))
        NormalFormTransducerTransition(
          from = (s, t2.from),
          to = (s, t2.to),
          in = None,
          out = Some((None, t2.out)),
          id = id
        )
      })
    )

    NormalFormTransducer[(State, StateB), InAlphabet, (Option[OutAlphabet], Option[OutAlphabetB])](
      start = (start, right.start),
      fin = fin.flatMap(f1 => right.fin.map(f2 => (f1, f2))),
      normalTransitions = nt.toSeq
    ).truncateUnReachable
  }


  def stateAny() =
    transducer.NormalFormTransducer[Any, InAlphabet, OutAlphabet](
      start, fin.toSet, normalTransitions.map(e => NormalFormTransducerTransition(e.from, e.to, e.in, e.out, e.id))
    )

  def outToNFA: NFA[State, OutAlphabet] =
    NFA(
      start = start,
      fin = fin,
      transitions = normalTransitions.map(t => Transition(t.from, t.to, t.out, t.id))
    )


  def mapOut[To](f: OutAlphabet => To) = NormalFormTransducer[State, InAlphabet, To](
    start = start,
    fin = fin,
    normalTransitions = normalTransitions.map(t => NormalFormTransducerTransition(
      from = t.from,
      to = t.to,
      in = t.in,
      out = t.out.flatMap(e => Some(f(e))),
      id = t.id
    ))
  )

  def mapTrans[To](f: NormalFormTransducerTransition[State, InAlphabet, OutAlphabet] => NormalFormTransducerTransition[State, InAlphabet, To]) =
    NormalFormTransducer[State, InAlphabet, To](
      start = start,
      fin = fin,
      normalTransitions = normalTransitions.map(f)
    )

}

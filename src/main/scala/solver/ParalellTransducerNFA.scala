package solver

import automaton.{EPSFreeNFA, NFA, Transition}
import transducer.*
import graph.EdgeUseCountVar
import util.EdgeUseCountTracker

import scala.collection.mutable.ListBuffer

class ParalellTransducerNFA[StateA, StateT,StateB,InAlphabet, OutAlphabet]
(
  in: NFA[StateA, InAlphabet],
  trans: NormalFormTransducer[StateT, InAlphabet, OutAlphabet],
  out: NFA[StateB, OutAlphabet],
)(
  implicit tracker: EdgeUseCountTracker = EdgeUseCountTracker()
) extends NFA[(StateA, StateT, StateB),  (Option[InAlphabet], Option[OutAlphabet])] ({
  type NewTransition = Transition[(StateA, StateT, StateB), (Option[InAlphabet], Option[OutAlphabet])]

  val nt = ListBuffer[NewTransition]()

  nt ++=
    in.transitions.filter(t1=>t1.in.isDefined).flatMap(t1 =>
      trans.normalTransitions.filter(t2=>t2.in==t1.in && t2.out.isDefined).flatMap(t2 =>
        out.transitions.filter(t3=>t3.in==t2.out).map(t3 => {
          val id = s"prt(${t1.id},${t2.id},${t3.id})"
          tracker.AddPart(EdgeUseCountVar(t1.id), EdgeUseCountVar(id))
          tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))
          tracker.AddPart(EdgeUseCountVar(t3.id), EdgeUseCountVar(id))
          Transition(
            from = (t1.from, t2.from, t3.from),
            to = (t1.to, t2.to, t3.to),
            in = (t1.in, t3.in),
            id = id
          )
        })
      )
    )

  nt ++=
    in.transitions.filter(t1 => t1.in.isDefined).flatMap(t1 =>
      trans.normalTransitions.filter(t2 => t2.in == t1.in && t2.out.isEmpty).flatMap(t2 =>
        out.states.map(s3 => {
          val id = s"prtT2isNone(${t1.id},${t2.id},${s3})"
          tracker.AddPart(EdgeUseCountVar(t1.id), EdgeUseCountVar(id))
          tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))
          Transition(
            from = (t1.from, t2.from, s3),
            to = (t1.to, t2.to, s3),
            in = (t1.in, None),
            id = id
          )
        })
      )
    )

  nt ++=
    in.states.flatMap(s1 =>
      trans.normalTransitions.filter(t2 => t2.in.isEmpty && t2.out.isDefined).flatMap(t2 =>
        out.transitions.filter(t3 => t3.in == t2.out).map(t3 => {
          val id = s"prtT1isNone(${s1},${t2.id},${t3.id})"
          tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))
          tracker.AddPart(EdgeUseCountVar(t3.id), EdgeUseCountVar(id))
          Transition(
            from = (s1, t2.from, t3.from),
            to = (s1, t2.to, t3.to),
            in = (None, t3.in),
            id = s"prtT1isNone(${s1},${t2.id},${t3.id})"
          )
        })
      )
    )

  nt ++=
    in.states.flatMap(s1 =>
      trans.normalTransitions.filter(t2 => t2.in.isEmpty && t2.out.isEmpty).flatMap(t2 =>
        out.states.map(s3 => {
          val id = s"prtTisNone(${s1},${t2.id},${s3})"
          tracker.AddPart(EdgeUseCountVar(t2.id), EdgeUseCountVar(id))
          Transition(
            from = (s1, t2.from, s3),
            to = (s1, t2.to, s3),
            in = (None, None),
            id = s"prtTisNone(${s1},${t2.id},${s3})"
          )
        })
      )
    )

  nt ++=
    in.transitions.filter(t1=>t1.in.isEmpty).flatMap(t1 =>
      trans.states.flatMap(s2 =>
        out.states.map(s3 => {
          val id = s"prtInNoneMove(${t1.id},${s2},${s3})"
          tracker.AddPart(EdgeUseCountVar(t1.id), EdgeUseCountVar(id))
          Transition(
            from = (t1.from, s2, s3),
            to = (t1.to, s2, s3),
            in = (None, None),
            id = s"prtInNoneMove(${t1.id},${s2},${s3})"
          )
        })
      )
    )

  nt ++=
    in.states.flatMap(s1 =>
      trans.states.flatMap(s2 =>
        out.transitions.filter(t3=>t3.in.isEmpty).map(t3 => {
          val id = s"prtOutNoneMove(${s1},${s2},${t3.id})"
          tracker.AddPart(EdgeUseCountVar(t3.id), EdgeUseCountVar(id))
          Transition(
            from = (s1, s2, t3.from),
            to = (s1, s2, t3.to),
            in = (None, None),
            id = s"prtOutNoneMove(${s1},${s2},${t3.id})"
          )
        })
      )
    )

  EPSFreeNFA(
    start = (in.start, trans.start, out.start),
    fin = in.fin.flatMap(af=>trans.fin.flatMap(tf=>out.fin.map(bf=>(af,tf,bf)))),
    epsFreeTransitions = nt.toSeq
  ).truncateUnReachable
}) {

}
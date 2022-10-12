package transducer

import dataType.*
import graph.{DirectedGraph, Edge, EdgeId, EdgeLike, UniqueEdgeId}
import automaton.{ParikhAutomaton, Transition, consumable, consume, NFA}
import presburger.ExistentialPresburgerFormula

import scala.:+
import util.EdgeUseCountTracker

type State = String

case class TransducerTransition[State, InAlphabet, Label]
  (from: State, to: State, in: InAlphabet, out: Label, id: EdgeId)
  extends EdgeLike[State] {
  override def toString: _root_.java.lang.String = s"$in / $out\n$id"
}

class EPSFreeTransducer[State, InAlphabet, OutMonoid: Monoid]
(
  start: State,
  fin: Set[State],
  transitions: Seq[TransducerTransition[State, InAlphabet, OutMonoid]],
) extends Transducer[State, InAlphabet, OutMonoid](
  start,fin, transitions.map(t=>TransducerTransition(t.from,t.to,Some(t.in), t.out, t.id))
)

class Transducer[State, InAlphabet, OutMonoid: Monoid]
(
  val start: State,
  val fin: Set[State],
  val transitions: Seq[TransducerTransition[State, Option[InAlphabet], OutMonoid]],
) extends DirectedGraph[State, TransducerTransition[State, Option[InAlphabet], OutMonoid]](transitions) {
  private val outM = implicitly[Monoid[OutMonoid]]
  type T = TransducerTransition[State, Option[InAlphabet], OutMonoid]

  def accept(word: Seq[InAlphabet]): Option[OutMonoid] = {
    val reached = Set[(State, Seq[InAlphabet])]()

    def f(word: Seq[InAlphabet], from: State = start): Option[OutMonoid] = {
      if (reached.contains((from, word)))
        None
      else {
        if (word.isEmpty) {
          if (fin.contains(from))
            Some(outM.unit)
          else
            None
        }
        else
          sourceFrom(from)
            .filter(trans => word.consumable(trans.in))
            .collectFirst(trans => f(word.consume(trans.in), trans.to) match {
              case Some(c) => c
            })
      }
    }

    f(word)
  }

  def addPrefix(prefix: String) = Transducer(
    start = (prefix,start),
    fin = fin.map(f => (prefix,f)),
    transitions = transitions.map(t =>
      TransducerTransition(
        from = (prefix,t.from),
        to = (prefix,t.to),
        in = t.in,
        out = t.out,
        id = s"${prefix}_${t.id}",
      )
    )
  )

}

extension[State, InAlphabet, OutAlphabet] (trans: Transducer[State, InAlphabet, List[OutAlphabet]]) {
  def parikhAutomaton: ParikhAutomaton[State, OutAlphabet] = {
    val m = IntVectorMonoid[OutAlphabet]()
    ParikhAutomaton[State, OutAlphabet](
      trans.start, trans.fin, trans.transitions.map(t => Transition(
        t.from, t.to, t.out.groupMapReduce(a=>a)(_=>1)((l,r)=>l+r): IntVector[OutAlphabet] , t.id
      ))
    )(m)
  }

  def normalForm: NormalFormTransducer[(State,String), InAlphabet, OutAlphabet] = {
    val newTransitions = trans.transitions.flatMap(t =>
      t.out match
        case List() =>
          List(NormalFormTransducerTransition(
            from = (t.from, ""),
            to = (t.to, ""),
            in = t.in,
            out = None,
            id = t.id,
          ))
        case _ =>
          t.out.zipWithIndex.map( (ch, idx) =>
            NormalFormTransducerTransition(
              from = if(idx == 0) (t.from, "") else (t.from, s"(inter_${t.id}_$idx)"),
              to = if(idx == t.out.length-1) (t.to, "") else (t.from, s"(inter_${t.id}_${idx + 1})"),
              in = if(idx == 0) t.in else None,
              out = Some(ch),
              id = if(idx == 0) t.id else s"nf_${t.id}/${idx}",
            )
          )
    )

    NormalFormTransducer(
      (trans.start, ""), trans.fin.map(s=>(s, "")), newTransitions
    )
  }

}

def transducerInToNFA[State, InAlphabet, OutAlphabet](t: Transducer[State, InAlphabet, OutAlphabet]): NFA[State, InAlphabet] =
  NFA(
    start = t.start,
    fin = t.fin,
    transitions = t.transitions.map(t=>Transition(t.from,t.to,t.in,t.id))
  )

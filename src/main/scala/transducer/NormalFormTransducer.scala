package transducer

import dataType.*
import graph.{EdgeId, EdgeLike}
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

  def combine[StateB, OutAlphabetB]
  (
    other: NormalFormTransducer[StateB, OutAlphabet, OutAlphabetB],
    tracker: EdgeUseCountTracker
  ):
    NormalFormTransducer[(State, StateB), InAlphabet, OutAlphabetB] = {

    val newTransitions = ListBuffer[NormalFormTransducerTransition[(State, StateB), InAlphabet, OutAlphabetB]]()

    newTransitions ++= normalTransitions.flatMap(t1 =>
      other.normalTransitions
        .filter(t2=>t2.in==t1.out&&t2.in.isDefined)
        .map[NormalFormTransducerTransition[(State, StateB), InAlphabet, OutAlphabetB]](t2 => {
          val id = s"(${t1.id},${t2.id})"
          tracker.AddPart(edgeUseCountVar(t1.id), edgeUseCountVar(id))
          tracker.AddPart(edgeUseCountVar(t2.id), edgeUseCountVar(id))

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
        tracker.AddPart(edgeUseCountVar(t1.id), edgeUseCountVar(id))

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
        tracker.AddPart(edgeUseCountVar(t2.id), edgeUseCountVar(id))

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


  override def addPrefix(prefix: String) = NormalFormTransducer(
    start = s"${prefix}_${start}",
    fin = fin.map(f => s"${prefix}_${f}"),
    normalTransitions = normalTransitions.map(t =>
      NormalFormTransducerTransition(
        from = s"${prefix}_${t.from}",
        to = s"${prefix}_${t.to}",
        in = t.in,
        out = t.out,
        id = s"${prefix}_${t.id}",
      )
    )
  )
}

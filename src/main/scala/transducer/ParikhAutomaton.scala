package transducer

import presburger.*
import graph.EdgeId

import scala.collection.mutable.ListBuffer

type IntVector[Key] = Map[Key, Int]

class IntVectorMonoid[Key] extends Monoid[IntVector[Key]] {
  override def plus(l: IntVector[Key], r: IntVector[Key]): IntVector[Key] = {
    (l.keys ++ r.keys).map(key => (key, l(key) + r(key))).toMap
  }
  override val unit = Map()
}

extension[OutAlphabet] (trans: Transducer[List[OutAlphabet]]) {
  def parikhAutomaton: ParikhAutomaton[OutAlphabet] = {
    val m = IntVectorMonoid[OutAlphabet]()
    ParikhAutomaton[OutAlphabet](
      trans.start, trans.fin, trans.transitions.map(t => Transition(
        t.from, t.to, t.in, t.out.groupMapReduce(a=>a)(_=>1)((l,r)=>l+r): IntVector[OutAlphabet] , t.id
      ))
    )(m)
  }

  def solve(constraint: ExistentialPresburgerFormula): Option[String] =
    parikhAutomaton.solve(constraint)
      .flatMap( edgeUseCount =>
        Some(trans.eulerTrail(trans.start, edgeUseCount).map(e =>
            trans.idToedgesMap(e.id).in
        ).mkString)
      )

  def findWrappedTrans(s: String): Seq[EdgeId] = {
    trans.transitions
      .filter(t =>
        s.substring(1, s.length - 1).contains(t.out.mkString)
      )
      .filter(t =>
        trans.sourceFrom(t.to).nonEmpty &&
        trans.targetTo(t.to).nonEmpty
      )
      .map(t => t.id)
  }
}

implicit class PrettyPrintMap[K, V](val map: Map[K, V]) {
  def prettyPrint: PrettyPrintMap[K, V] = this

  override def toString: String = {
    val valuesString = toStringLines.mkString("\n")

    "Map (\n" + valuesString + "\n)"
  }

  def toStringLines = {
    map
      .flatMap{ case (k, v) => keyValueToString(k, v)}
      .map(indentLine(_))
  }

  def keyValueToString(key: K, value: V): Iterable[String] = {
    value match {
      case v: Map[_, _] => Iterable(key.toString + " -> Map (") ++ v.prettyPrint.toStringLines ++ Iterable(")")
      case x => Iterable(key.toString + " -> " + x.toString)
    }
  }

  def indentLine(line: String): String = {
    "\t" + line
  }
}
class ParikhAutomaton[Key]
(
  start: State,
  fin: Set[State],
  transitions: Seq[Transition[State, IntVector[Key]]],
)(
  implicit outM: Monoid[IntVector[Key]]
) extends Transducer[IntVector[Key]] (
  start, fin, transitions
) {

  type T = Transition[State, IntVector[Key]]
  val keys: Set[Key] = transitions.flatMap(t=>t.out.keys).toSet

  def solve(constraint: ExistentialPresburgerFormula): Option[Map[EdgeId, Int]] = {
    PresburgerFormulaSolver().solve(And(
      constraint,
      chCountPresburgerFormula
    )) match {
      case Some(m) =>
//        println(m.prettyPrint)
        Some(transitions.map(trans =>
          (trans.id, m(transOccurCountVar(trans).name))
        ).toMap)
      case None =>
        print(PresburgerFormulaSolver().findUnSatCore(And(
          constraint,
          chCountPresburgerFormula
        )).get.enumerateVar)
        None
    }
  }

  def prefixForKeyCount = "alphabet_"
  val KeyCountVar = (key: Key) => Variable(prefixForKeyCount + key)

  def chCountPresburgerFormula: ExistentialPresburgerFormula = {
    val formulas = ListBuffer(pathConstraint)
    formulas ++= states.filter(s=>s!=start).map(s =>
        Equal(isStartVar(s), Constant(0))
    )
    formulas ++= states.diff(fin).map(s =>
      Equal(isFinVar(s), Constant(0))
    )
    formulas ++= keys.map(key =>
      Equal(
        transitions.map(t =>
          Mul(
            Constant(t.out.getOrElse(key, 0)),
            transOccurCountVar(t)
          )
        ).reduce(Add.apply),
        KeyCountVar(key)
      )
    )

    AndList(formulas.toSeq)
  }
}

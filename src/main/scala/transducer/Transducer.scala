package transducer


trait TransducerLike[Input, Output] {
def transduce(word: Input): Option[Output]
}

type State = String

case class Transition[State, InAlphabet, Label](from: State, to: State, in: InAlphabet, out: Label, id: Int)

trait Monoid[A] {
  def plus(l: A, r: A): A
  val unit: A
}

class Transducer[InAlphabet, OutMonoid: Monoid]
(
  val start: State,
  val fin: Set[State],
  val transitions: Set[Transition[State, InAlphabet, OutMonoid]],
) extends TransducerLike[List[InAlphabet], OutMonoid] {
  type T = Transition[State, InAlphabet, OutMonoid]

  private val outM = implicitly[Monoid[OutMonoid]]

  type InWord = List[InAlphabet]

  val states: Set[State] = transitions.flatMap(t => Set(t.from, t.to))

  val transitionMap: Map[State, Map[InAlphabet, T]] = {
    print("ini")
    transitions.groupBy(t => t.from).mapValues(v => v.map(t => (t.in, t)).toMap).toMap
  }

  override def transduce(word: InWord): Option[OutMonoid] = {
    val (finalState, output) = word.foldLeft[(State, OutMonoid)]((start, outM.unit))((s, a) => {
      val (state: State, outWord: OutMonoid) = s
      val trans = transitionMap(state)(a)
      (trans.to, outM.plus(outWord, trans.out))
    })

    if (fin.contains(finalState))
      Some(output)
    else None
  }

  def sourceFrom(q: State): Set[T] = {
    transitions.filter(t => t.from == q)
  }
  def targetTo(q: State): Set[T] = {
    transitions.filter(t => t.to == q)
  }
}

implicit object StringMonoid extends Monoid[String] {
  override val unit: String = ""

  override def plus(l: String, r: String): String = l ++ r
}
type StringTransducer = Transducer[Char, String]
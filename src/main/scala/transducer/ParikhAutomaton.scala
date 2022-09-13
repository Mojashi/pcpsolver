package transducer

import presburger.*

type IntVector[Key] = Map[Key, Int]

class IntVectorMonoid[Key] extends Monoid[IntVector[Key]] {
  override def plus(l: IntVector[Key], r: IntVector[Key]): IntVector[Key] = {
    (l.keys ++ r.keys).map(key => (key, l(key) + r(key))).toMap
  }
  override val unit = Map()
}

extension[InAlphabet, OutAlphabet] (trans: Transducer[InAlphabet, List[OutAlphabet]]) {
  def parikhAutomaton(alphabetSet: Set[InAlphabet]): ParikhAutomaton[InAlphabet, OutAlphabet] = {
    val len = alphabetSet.size

    val m = IntVectorMonoid[OutAlphabet]()
    ParikhAutomaton[InAlphabet, OutAlphabet](
      trans.start, trans.fin, trans.transitions.map(t => Transition(
        t.from, t.to, t.in, t.out.groupMapReduce(a=>a)(_=>1)((l,r)=>l+r): IntVector[OutAlphabet] , t.id
      ))
    )(m)
  }
}

class ParikhAutomaton[Alphabet, Key]
(
  start: State,
  fin: Set[State],
  transitions: Set[Transition[State, Alphabet, IntVector[Key]]],
)(
  implicit outM: Monoid[IntVector[Key]]
) extends Transducer[Alphabet, IntVector[Key]] (
  start, fin, transitions
) {

  type T = Transition[State, Alphabet, IntVector[Key]]


  def presburgerFormula: ExistentialPresburgerFormula = {
    val transOccurCountVar = (t: T) => Variable(s"y_${t.id}")
    val distanceVar = (q: State) => Variable(s"z_${q}")
    val stateOccurCountVar = (q: State) => Variable(s"n_${q}")
    val isStart = (q: State) => if(q == start) Constant(1) else Constant(0)
    val isFin = (q: State) => if(fin.contains(q)) Constant(1) else Constant(0)

    val SumYdTargetToQ = (q:State) => Variable(s"sum_y_target_${q}")
    val SumYdSourceFrQ = (q: State) => Variable(s"sum_y_source_${q}")

    val notReached = (q: State) => And(
      Equal(
        stateOccurCountVar(q),
        Constant(0)
      ),
      Equal(
        distanceVar(q),
        Constant(-1)
      )
    )
    val isReached = (q: State) =>
      And(
        GreaterThan(
          stateOccurCountVar(q),
          Constant(0)
        ),
        Or(
          And(
            Equal(
              distanceVar(q),
              Constant(0)
            ),
            Equal(
              isStart(q),
              Constant(1),
            )
          ),
          sourceFrom(q).map(t=>
            Equal(
              distanceVar(t.from),
              Add(
                distanceVar(q),
                Constant(1)
              )
            )
          ).reduce(Or.apply)
        )
    )

    val states = this.states.toSeq

    val formulas = states.map(q =>
      Equal(
        targetTo(q).map(transOccurCountVar).reduceLeft((l, r) => Add(l, r)),
        SumYdTargetToQ(q)
      )
    )++
    states.map(q =>
      Equal(
        sourceFrom(q).map(transOccurCountVar).reduce((l, r) => Add(l, r)),
        SumYdSourceFrQ(q)
      )
    )++
    states.map(q =>
      Equal(
        Add(
          Sub(
            SumYdTargetToQ(q),
            SumYdSourceFrQ(q)
          ),
          Sub(
            isStart(q),
            isFin(q)
          )
        ),
        Constant(0)
      )
    )++
    states.map(q =>
      Equal(
        stateOccurCountVar(q),
        Add(
          SumYdTargetToQ(q),
          isStart(q)
        )
      )
    )++
    states.map(q =>
      Equal(
        stateOccurCountVar(q),
        Add(
          SumYdSourceFrQ(q),
          isFin(q)
        )
      )
    ) ++
    states.map(q => Or(isReached(q) , notReached(q)))

    formulas.reduce(And.apply)
  }

}

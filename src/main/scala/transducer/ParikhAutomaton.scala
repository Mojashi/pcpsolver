package transducer

import presburger.*
import graph.EdgeId

type IntVector[Key] = Map[Key, Int]

class IntVectorMonoid[Key] extends Monoid[IntVector[Key]] {
  override def plus(l: IntVector[Key], r: IntVector[Key]): IntVector[Key] = {
    (l.keys ++ r.keys).map(key => (key, l(key) + r(key))).toMap
  }
  override val unit = Map()
}

extension[InAlphabet, OutAlphabet] (trans: Transducer[InAlphabet, List[OutAlphabet]]) {
  def parikhAutomaton: ParikhAutomaton[InAlphabet, OutAlphabet] = {
    val m = IntVectorMonoid[OutAlphabet]()
    ParikhAutomaton[InAlphabet, OutAlphabet](
      trans.start, trans.fin, trans.transitions.map(t => Transition(
        t.from, t.to, t.in, t.out.groupMapReduce(a=>a)(_=>1)((l,r)=>l+r): IntVector[OutAlphabet] , t.id
      ))
    )(m)
  }

  def solve(constraint: ExistentialPresburgerFormula): Option[List[InAlphabet]] =
    parikhAutomaton.solve(constraint)
      .flatMap( edgeUseCount =>
        Some(trans.graph.eulerTrail(trans.start, edgeUseCount).map(e =>
            trans.idToTransitionMap(e.id).in
        ))
      )
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
  val keys: Set[Key] = transitions.flatMap(t=>t.out.keys)

  def solve(constraint: ExistentialPresburgerFormula): Option[Map[EdgeId, Int]] = {
    PresburgerFormulaSolver().solve(And(
      constraint,
      presburgerFormula
    )).flatMap(m =>
      Some(transitions.map(trans =>
        (trans.id, m(transOccurCountVar(trans).name))
      ).toMap)
    )
  }

  private val transOccurCountVar = (t: T) => Variable(s"y_${t.id}")
  private val distanceVar = (q: State) => Variable(s"z_${q}")
  private val stateOccurCountVar = (q: State) => Variable(s"n_${q}")
  private val isStart = (q: State) => if (q == start) Constant(1) else Constant(0)
  private val isFin = (q: State) => if (fin.contains(q)) Constant(1) else Constant(0)

  def presburgerFormula: ExistentialPresburgerFormula = {
    val SumYdTargetToQ = (q: State) => Variable(s"sum_y_target_${q}")
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
            targetTo(q).map(t=>
              AndList(List(
                Equal(
                  distanceVar(q),
                  Add(
                    distanceVar(t.from),
                    Constant(1)
                  )
                ),
                GreaterThan(
                  transOccurCountVar(t),
                  Constant(0)
                ),
                GreaterThanOrEqual(
                  distanceVar(t.from),
                  Constant(0)
                )
              ))
            ).reduceOption(Or.apply).getOrElse(True),
        )
    )

    val states = this.states.toSeq

    val formulas: Seq[Option[ExistentialPresburgerFormula]] =
    transitions.toSeq.map(t =>
        Some(GreaterThanOrEqual(transOccurCountVar(t), Constant(0)))
    ) ++
    states.map(q =>
      Some(Equal(
        targetTo(q).map(transOccurCountVar).reduce((l, r) => Add(l, r)),
        SumYdTargetToQ(q)
      ))
    )++
    states.map(q =>
      sourceFrom(q).map(transOccurCountVar)
        .reduceOption[PresburgerExpression]((l, r) => Add(l, r)).flatMap(e =>
        Some(Equal(
          e,
          SumYdSourceFrQ(q)
        ))
      )) ++
    states.map(q =>
      Some(Equal(
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
      ))
    )++
    states.map(q =>
      Some(Equal(
        stateOccurCountVar(q),
        Add(
          SumYdTargetToQ(q),
          isStart(q)
        )
      ))
    )++
    states.map(q =>
      Some(Equal(
        stateOccurCountVar(q),
        Add(
          SumYdSourceFrQ(q),
          isFin(q)
        )
      ))
    ) ++
    states.map(
      q => Some(Or(isReached(q) , notReached(q)))
    ) ++
    keys.map(key =>
        Some(Equal(
        transitions.map(t =>
          Mul(
            Constant(t.out.getOrElse(key, 0)),
            transOccurCountVar(t)
          )
        ).reduce(Add.apply),
        Variable(key.toString)
      ))
    )

    formulas.flatten.reduce(And.apply)
  }



}

package transducer

import presburger.*

class VertexOrientedTransducer
(
  val baseTransducer: Transducer[List[Char]]
) extends Transducer(
  start = "start",
  fin = baseTransducer.fin.flatMap(finState => baseTransducer.targetTo(finState).map(trans => trans.id)),
  transitions =
    baseTransducer.transitions.flatMap(trans =>
      baseTransducer.sourceFrom(trans.to).map(nextTrans =>
        Transition(
          from = trans.id,
          to = nextTrans.id,
          in = nextTrans.in,
          out = nextTrans.out,
          id = s"${trans.id}_${nextTrans.id}"
        )
      )
    ) ++
    baseTransducer.sourceFrom(baseTransducer.start)
      .map(nextTrans =>
        Transition(
          from = "start",
          to = nextTrans.id,
          in = nextTrans.in,
          out = nextTrans.out,
          id = s"start_${nextTrans.id}"
        )
      )
)(ListMonoid[Char]()) {
  val vertexStr: Map[State, String] =
    baseTransducer.transitions.map(trans => (trans.id, trans.out.mkString)).concat(List((start, ""))).toMap

  def substrCountVar(s: String) = Variable(s"substr_${s}")

  def extraPresburgerFormulaWithSubstrCount(targets: Set[String]): ExistentialPresburgerFormula = {
    val pa = this.parikhAutomaton
    val newFormula = targets.toList.map(t => {
      val counter = transitions.toList
        .map(trans => ((vertexStr(trans.from) + vertexStr(trans.to)).countSubstring(t) - vertexStr(trans.from).countSubstring(t), trans))
        .filter({ case (count, _) => count > 0 })
        .map({ case (count, trans) =>
          Mul(
            Constant(count),
            pa.transOccurCountVar(pa.idToedgesMap(trans.id)),
          )
        })
        .foldLeft[PresburgerExpression](Constant(0))((cum, cur) => Add(cum, cur))

      Equal(
        counter,
        substrCountVar(t)
      )
    })

    AndList(newFormula)
  }

  def presburgerFormulaWithSubstrCount(targets: Set[String]): ExistentialPresburgerFormula =
    And(
      extraPresburgerFormulaWithSubstrCount(targets),
      this.parikhAutomaton.chCountPresburgerFormula
    )

  def solve(constraint: ExistentialPresburgerFormula, targets: Set[String]): Option[String] =
    this.parikhAutomaton.solve(And(constraint, extraPresburgerFormulaWithSubstrCount(targets)))
      .flatMap(edgeUseCount =>
        Some(eulerTrail(start, edgeUseCount).map(e =>
          idToedgesMap(e.id).in
        ).mkString)
      )
}

extension (s: String) {
  def countSubstring(substr: String):Int = {
    s.indices.count(begin => s.substring(begin).startsWith(substr))
  }
}
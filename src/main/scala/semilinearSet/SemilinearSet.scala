package semilinearSet

import dataType.{IntVector, IntVectorMonoid}

// OrのStar→base一緒にしていい
case class LinearSet[Key]
(
  val constant: IntVector[Key],
  val bases: Set[IntVector[Key]],
) {
  private val m = IntVectorMonoid[Key]()

  def add(right: LinearSet[Key]): LinearSet[Key] = LinearSet[Key](
    constant = m.plus(constant, right.constant),
    bases = bases.union(right.bases)
  )

  def solve(target: IntVector[Key]) = {

  }
}

case class SemiLinearSet[Key]
(
  val sets: Set[LinearSet[Key]]
) {
  def add(right: SemiLinearSet[Key]): SemiLinearSet[Key] = SemiLinearSet[Key](
    sets = right.sets.flatMap(r => this.add(r).sets)
  )

  def add(right: LinearSet[Key]): SemiLinearSet[Key] = SemiLinearSet[Key](
    sets = sets.map(s => s.add(right))
  )

  def union(right: SemiLinearSet[Key]): SemiLinearSet[Key] = SemiLinearSet[Key](
    sets = sets.union(right.sets)
  )

  def solve(target: IntVector[Key]) = sets.collectFirst(s => s.solve(target))
}
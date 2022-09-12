package pcp

import transducer.{Monoid, StringTransducer, Transducer, Transition, ListMonoid}
case class Tile(u: String, d: String)

class PCP
(
  tiles: List[Tile]
) {
  def transducers: (Transducer[Int, List[Char]],Transducer[Int, List[Char]]) =
    (toTransducer(tiles.map(t=>t.u)), toTransducer(tiles.map(t=>t.d)))
}

def toTransducer(ss: List[String]): Transducer[Int, List[Char]] =
  Transducer(
    "q", Set("q"), ss.zipWithIndex.map({ case (s, idx) =>
      Transition("q", "q", idx, s.toList, idx)
    }).toSet
  )(ListMonoid[Char]())

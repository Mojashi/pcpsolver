package pcp

import transducer.*
import presburger.*
import dataType.*

import scala.collection.mutable
case class Tile(u: String, d: String)

class PCP
(
  val tiles: List[Tile]
) {
  val alphabets = tiles.flatMap(tile=>tile.d++tile.u).toSet
  def transducers: (Transducer[String, Int, List[Char]],Transducer[String, Int, List[Char]]) =
    (toTransducer(tiles.map(t=>t.u)), toTransducer(tiles.map(t=>t.d)))

  def transduce(word: Seq[Int]): (String, String) = 
    (
      word.foldLeft("")((s, idx) => s + tiles(idx).u),
      word.foldLeft("")((s, idx) => s + tiles(idx).d),
    )

  def solveCommonParikhImage: Option[Seq[Int]] = {
    val (at, bt) = transducers
    val ap = at.parikhAutomaton

    bt.solveInputWord(And(
      ap.chCountPresburgerFormula,
      // forbid empty
      GreaterThan(
        alphabets.map(ch => ap.KeyCountVar(ch)).reduce((l,r)=>Add(l,r)),
        Constant(0)
      )
    )).flatMap(s=>Some(s.map(ch=>ch.toString.toInt)))
  }
}

def toTransducer(ss: List[String]): EPSFreeTransducer[String, Int, List[Char]] =
  EPSFreeTransducer(
    "q", Set("q"), ss.zipWithIndex.map({ case (s, idx) =>
      TransducerTransition[String, Int, List[Char]](s"q", "q", idx, s.toList, s"$idx")
    })
  )(ListMonoid[Char]())

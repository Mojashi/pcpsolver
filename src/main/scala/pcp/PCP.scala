package pcp

import transducer.*
import presburger.*
import dataType.*
import graph.{UniqueEdgeId, EdgeUseCountVar}
import util.*

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

  def solveCommonSubstrings(words: Seq[String], maxLen: Option[Int]): Option[Seq[Int]] = {
    val (rt1, rt2) = transducers
    val (t1, t2) = (
      rt1.addPrefix("t1").normalForm.mapEither(),
      rt2.addPrefix("t2").normalForm.mapEither()
    )

    val ts: Seq[NormalFormTransducer[String, Either[Char, String], Either[Char, String]]] = words.zipWithIndex.map((s, idx) =>
      SubStrCountTrans(s, alphabets, words.take(idx).map(p => Right(p)).toSet)
    )
    ts.zipWithIndex.foreach((t, idx) => t.saveSVG(s"ts$idx"))

    implicit val tracker = EdgeUseCountTracker()

    var tt1: NormalFormTransducer[Any, Int, Either[Char, String]] = t1.stateAny()
    var tt2: NormalFormTransducer[Any, Int, Either[Char, String]] = t2.stateAny()

    ts.foreach(t => {
      tt1 = tt1.combine(t.addPrefix("a")).stateAny()
      tt2 = tt2.combine(t.addPrefix("a")).stateAny()
    })

    val tt = tt1.product(tt2)
//    tt.saveSVG("tt")
    var formula = AndList(List(
      tt.acceptConstraint,
      tt.parikhAutomaton.chCountPresburgerFormula,
      GreaterThan(
        //      Variable("y_t1_2"),
        Variable(s"sum_y_source_${tt.start}"),
        Constant(0),
      ),
//      AndList(words.map(w =>
//        Equal(
//          tt.parikhAutomaton.KeyCountVar((Some(Right(w)), None)),
//          tt.parikhAutomaton.KeyCountVar((None, Some(Right(w))))
//        )
//      )),
    ))

//    t1.saveSVG("t1")
    formula = And(formula, tracker.formula(formula.enumerateVar))

    val ss = alphabets.flatMap(a1 => alphabets.map(a2 =>
      (Some(Left(a1)), Some(Left(a2)))
    ) ++ Seq((Some(Left(a1)), None))
    ).map(s=>tt.parikhAutomaton.KeyCountVar(s)).intersect(formula.enumerateVar.map(v=>Variable(v))).fold[PresburgerExpression](Constant(0))((l,r)=>Add(l,r))

    tt.solveInputWord(
      AndList(List(
        formula,

        maxLen.flatMap(maxLen =>
          Some(GreaterThanOrEqual(
            Constant(maxLen),
            ss
          ))
        ).getOrElse(True)

      ))
    )

  }

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

def SubStrCountTrans(word: String, alphabets: Set[Char], ignoreAlphabets: Set[Either[Char, String]]): NormalFormTransducer[String, Either[Char, String], Either[Char, String]] = {
  NormalFormTransducer[String, Either[Char, String], Either[Char, String]](
    start = s"0",
    fin = word.indices.map(idx => s"$idx").toSet,
    normalTransitions = word.indices.flatMap(idx =>
      alphabets.toSeq.map(other => {
        val newWord = word.substring(0, idx) + other
        val jumpTo = (0 to newWord.length).findLast(l => word.substring(0, l) == newWord.substring(newWord.length - l, newWord.length)).get

        NormalFormTransducerTransition(s"$idx", s"${jumpTo}", Some(Left(other)), Some(Left(other)), UniqueEdgeId.get)
      }) ++
        ignoreAlphabets.toSeq.map(a =>
          NormalFormTransducerTransition(s"$idx", s"$idx", Some(a), Some(a), UniqueEdgeId.get)
        )
    ) ++
      Seq(NormalFormTransducerTransition(
        s"${word.length}", "0", None, Some(Right(word)), UniqueEdgeId.get
      ))
  )
}

extension[State, InAlphabet, OutAlphabet](t: NormalFormTransducer[State, InAlphabet, OutAlphabet]) {
  def mapTransition[InAlphabetB, OutAlphabetB]
  (f: NormalFormTransducerTransition[State, InAlphabet, OutAlphabet]=> NormalFormTransducerTransition[State, InAlphabetB, OutAlphabetB]): NormalFormTransducer[State, InAlphabetB, OutAlphabetB] = {
    NormalFormTransducer(
      start = t.start,
      fin = t.fin,
      normalTransitions = t.normalTransitions.map(f)
    )
  }

  def mapEither() =
    t.mapTransition[InAlphabet, Either[OutAlphabet,String]](t=>
      NormalFormTransducerTransition(t.from,t.to,t.in, t.out.flatMap(c=>Some(Left(c))), t.id )
    )

  def stateAny() =
    transducer.NormalFormTransducer[Any, InAlphabet, OutAlphabet](
      t.start,t.fin.toSet,t.normalTransitions.map(e=>NormalFormTransducerTransition(e.from,e.to,e.in,e.out,e.id))
    )

}

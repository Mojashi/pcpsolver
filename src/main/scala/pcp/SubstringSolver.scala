package pcp

import graph.UniqueEdgeId
import presburger.*
import transducer.*
import util.*

object SubstringSolver extends App {
    val pcp = PCP(List(
      Tile("100", "101"),
      Tile("0", "1"),
      Tile("1", "0"),
    ))

  val (rt1,rt2) = pcp.transducers
  val (t1,t2) = (
    rt1.addPrefix("t1").normalForm.mapEither(),
    rt2.addPrefix("t1").normalForm.mapEither()
  )

  val words = Seq("10")

  val ts: Seq[NormalFormTransducer[String, Either[Char, String], Either[Char, String]]] = words.zipWithIndex.map((s, idx) =>
    SubStrCountTrans(s, pcp.alphabets.map(c=>Left(c)), words.take(idx).map(p=>Right(p)).toSet)
  )

  var tt1 = t1.stateAny()
  var tt2 = t2.stateAny()
  val tracker = EdgeUseCountTracker()
  ts.foreach(t=> {
    tt1 = tt1.combine(t, tracker).stateAny()
    tt2 = tt2.combine(t, tracker).stateAny()
  })

  val ans = tt2.solveInputWord(AndList(List(
    tt1.acceptConstraint,
    tt2.acceptConstraint,
    tt1.parikhAutomaton.chCountPresburgerFormula,
    tt2.parikhAutomaton.chCountPresburgerFormula,
    GreaterThan(
      tt1.parikhAutomaton.KeyCountVar(Left('0')),
      Constant(0)
    ),
    tracker.formula,
    AndList(pcp.tiles.indices.map(i =>
      Equal(
        Variable(s"y_t1_$i"),
        Variable(s"y_t1_$i"),
      ),
    ))
  )))


  println(tracker.parts.toMap.prettyPrint)
  println(tracker.formula)

  tt1.parikhAutomaton.saveSVG("tt1.pa")
  tt2.parikhAutomaton.saveSVG("tt2.pa")
  tt1.saveSVG("tt1")
  tt2.saveSVG("tt2")
//  ts(0).saveSVG("ts0")

  println(ans)
  println(pcp.transduce(ans.get))

  def SubStrCountTrans(word: String, alphabets: Set[Either[Char, String]], ignoreAlphabets: Set[Either[Char, String]]): NormalFormTransducer[String, Either[Char, String], Either[Char, String]] = {
    NormalFormTransducer[String, Either[Char, String], Either[Char, String]](
      start = s"0",
      fin = Set(s"0"),
      normalTransitions = word.zipWithIndex
        .flatMap((ch, idx) =>
          List(
            NormalFormTransducerTransition(s"$idx", s"${idx+1}", Some(Left(ch)), Some(Left(ch)), UniqueEdgeId.get)
          ) ++
          alphabets.filter(other=>other!=Left(ch)).toSeq.map( other =>
            NormalFormTransducerTransition(s"$idx", s"0", Some(other), Some(other), UniqueEdgeId.get)
          ) ++
          ignoreAlphabets.toSeq.map(a =>
            NormalFormTransducerTransition(s"$idx", s"$idx", Some(a), None, UniqueEdgeId.get)
          )
        ) ++
        Seq(NormalFormTransducerTransition(
          s"${word.length}", "0", None, Some(Right(word)), UniqueEdgeId.get
        ))
    )
  }


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

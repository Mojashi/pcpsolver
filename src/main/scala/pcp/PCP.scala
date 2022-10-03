package pcp

import transducer.*
import presburger.*
import dataType.*
import graph.{EdgeUseCountVar, UniqueEdgeId}
import automaton.{NFA, Transition}
import solver.ParallelNFA
import util.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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

  def solveWithWathcer(watcher: NFA[Any, Char], maxLen: Option[Int]): Option[Seq[Int]] = {
    val (rt1, rt2) = transducers
    val (t1, t2) = (
      rt1.addPrefix("t1").normalForm,
      rt2.addPrefix("t2").normalForm
    )

    implicit val tracker = EdgeUseCountTracker()

    val tt1 = t1.combine(watcher)
    val tt2 = t2.combine(watcher)
    val tt = ParallelNFA(tt1, tt2)
    tt1.saveSVG("tt1")
    tt2.saveSVG("tt2")

    println(s"watcher.transitions.size=${watcher.transitions.size}")
    println(s"tt.transitions.size=${tt.transitions.size}")

    var formula = AndList(List(
      tt.acceptConstraint,
      GreaterThan(
        Variable(s"sum_y_source_${tt.start}"),
        Constant(0),
      ),
    ))

    formula = And(formula, tracker.formula(formula.enumerateVar))

    val ss = tiles.indices.map(s => tt.parikhAutomaton.KeyCountVar(s)).fold[PresburgerExpression](Constant(0))((l, r) => Add(l, r))

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

  def solveCommonSubstrings(words: Seq[String], alignPrefLen: Int, maxLen: Option[Int]): Option[Seq[Int]] = {
    val ts: Seq[NFA[Any, Char]] = words.map(s =>SubStrCountNFA(s, alphabets).stateAny())
    ts.zipWithIndex.foreach((t, idx) => t.saveSVG(s"ts$idx"))

    var wathcer = ts.reduce((l,r)=>ParallelNFA(l,r).stateAny())

    if(alignPrefLen > 0) {
      wathcer = addPrefixToNFA(wathcer, alignPrefLen, alphabets).stateAny()
    }

    wathcer.saveSVG("watcher")

    solveWithWathcer(wathcer, maxLen)
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

def SubStrCountNFA(word: String, alphabets: Set[Char]): NFA[String,Char] = {
  NFA[String, Char](
    start = s"0",
    fin = word.indices.map(idx => s"$idx").toSet,
    transitions = word.indices.flatMap(idx =>
      alphabets.toSeq.map(other => {
        val newWord = word.substring(0, idx) + other
        val jumpTo = (0 to newWord.length).findLast(l => word.substring(0, l) == newWord.substring(newWord.length - l, newWord.length)).get

        Transition(s"$idx", s"${jumpTo}", Some(other), UniqueEdgeId.get)
      })
    ) ++
      Seq(Transition(
        s"${word.length}", "0", None, UniqueEdgeId.get
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
}

def addPrefixToNFA[State](nfa: NFA[State, Char], prefixLen: Int, alphabets: Set[Char]): NFA[(String, Any), Char] = {
  val transitions = ListBuffer[Transition[(String, Any), Option[Char]]]()

  def dfs(curState: String, rest: Int): Unit = {
    if (rest == 0) {
      transitions.addAll(alphabets.map(ch =>
        Transition(
          from = ("pref", curState),
          to = ("", nfa.start),
          in = Some(ch),
          id = UniqueEdgeId.get
        )
      ))
    } else {
      alphabets.foreach(ch => {
        val nextState = curState + ch
        transitions.addOne(
          Transition(
            from = ("pref", curState),
            to = ("pref", nextState),
            in = Some(ch),
            id = UniqueEdgeId.get
          )
        )

        dfs(nextState, rest - 1)
      })
    }
  }

  dfs("", prefixLen - 1)


  NFA[(String, Any), Char](
    start = ("pref", ""),
    fin = nfa.fin.map(f => ("", f)),
    transitions = nfa.transitions.map[Transition[(String, Any), Option[Char]]](t =>
      Transition(
        from = ("", t.from),
        to = ("", t.to),
        in = t.in,
        id = t.id
      )
    ) ++ transitions
  )
}

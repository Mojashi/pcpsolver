package pcp

import transducer.*
import transducer.NormalFormTransducer
import presburger.*
import regex.{simplifyWithOutIncrease, toRegex}
import dataType.*
import graph.{EdgeUseCountVar, UniqueEdgeId}
import automaton.{EPSFreeNFA, NFA, ParikhAutomaton, Transition, solveParikhImageToZeroWithLP}
import solver.ParallelNFA
import util.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
case class Tile(u: String, d: String)

case class PCP
(
  val tiles: Seq[Tile]
) {
  System.loadLibrary("jniortools")

  val alphabets = tiles.flatMap(tile=>tile.d++tile.u).toSet
  def transducers: (Transducer[String, Int, List[Char]],Transducer[String, Int, List[Char]]) =
    (toTransducer(tiles.map(t=>t.u).toList), toTransducer(tiles.map(t=>t.d).toList))

  def transduce(word: Seq[Int]): (String, String) = (
      word.foldLeft("")((s, idx) => s + tiles(idx).u),
      word.foldLeft("")((s, idx) => s + tiles(idx).d),
    )

  def solveWithParikhImageWatcher[Out](watcher: NormalFormTransducer[Any, Char, IntVector[Out]], maxLen: Option[Int]): Option[Seq[Int]] = {
    val (rt1, rt2) = transducers
    val (t1, t2) = (
      rt1.addPrefix("t1").normalForm,
      rt2.addPrefix("t2").normalForm
    )

    val tt1 = t1.combine(watcher)
    val tt2 = t2.combine(watcher)

    val m = IntVectorMonoid[Out]()
    val tt = tt1.product(tt2).mapOut {
      case (Some(e1), Some(e2)) => m.plus(e1, e2.map((key, count) => (key, -count)))
      case (Some(e1), None) => e1
      case (None, Some(e2)) => e2.map((key, count) => (key, -count))
      case (None, None) => m.unit
    }.stateAny()

    println(s"watcher.transitions.size=${watcher.transitions.size}")
    println(s"tt.transitions.size=${tt.transitions.size}")

    val pa = ParikhAutomaton(
      start = tt.start,
      fin = tt.fin,
      transitions = tt.normalTransitions.map(t => Transition(t.from, t.to, t.out.getOrElse(m.unit), t.id))
    )(m)

    val yuru = solveParikhImageToZeroWithLP(pa, false)
    if(yuru.isEmpty) {
      println(s"linear relaxed unsat")
      return None
    }
    return Some(Seq())

    println(s"relax ok")

    val lpres = solveParikhImageToZeroWithLP(pa, true)
//    println(s"lpres: ${lpres.isDefined}")
    if(lpres.isDefined) {
      return Some(tt.eulerTrail(pa.start, lpres.get).get.flatMap(t => t.in))
    } else {
      return None
    }

    val res = transducerInToNFA(tt).solveInputWord(AndList(Seq(
      pa.chCountPresburgerFormula,
      pa.acceptConstraint,
      GreaterThan(
        Variable(s"sum_y_source_${pa.start}"),
        Constant(0),
      ),
    ) ++ pa.keys.toSeq.map(key =>
      Equal(pa.KeyCountVar(key), Constant(0))
    )))

    res
  }

  def solveWithWatcher(watcher: NFA[Any, Char], maxLen: Option[Int]): Option[Seq[Int]] = {
    val wathcerTransducer = NormalFormTransducer(
      start = watcher.start,
      fin = watcher.fin,
      normalTransitions = watcher.transitions.map(t => NormalFormTransducerTransition(
        t.from, t.to, t.in, Some(t.id), t.id
      ))
    )
    wathcerTransducer.saveSVG("wathcerTransducer")

    val ret = solveWithWatcher(wathcerTransducer, maxLen)

    //    if(ret.isDefined) {
    //      val (o1, o2) = transduce(ret.get)
    ////      println(watcher.accept(o1.toList))
    ////      println(watcher.accept(o2.toList))
    //      val u1 = watcher.getUseCount(o1.toList)
    //      val u2 = watcher.getUseCount(o2.toList)
    //      println(u1.prettyPrint)
    //      println(u2.prettyPrint)
    //    }

    ret
  }

  def solveWithWatcher[Out](watcher: NormalFormTransducer[Any, Char, Out], maxLen: Option[Int]): Option[Seq[Int]] = {
    val parikhWatcher = watcher.mapOut(out => Map((out, 1)))
    val res = solveWithParikhImageWatcher(parikhWatcher, maxLen)
    res
  }

  def solveWithWatcherViaCone(watcher: NFA[Any, Char]): Boolean = {
    val watcherTransducer = (v: Int) => NormalFormTransducer[Any, Char, IntVector[EdgeId]](
      start = watcher.start,
      fin = watcher.fin,
      normalTransitions = watcher.transitions.map(t => NormalFormTransducerTransition(
        from = t.from,
        to = t.to,
        in = t.in,
        out = Some(Map((t.id, v)): IntVector[EdgeId]),
        id = t.id,
      ))
    )

    val (rt1, rt2) = transducers
    val (t1, t2) = (
      rt1.addPrefix("t1").normalForm,
      rt2.addPrefix("t2").normalForm
    )

    implicit val tracker = EdgeUseCountTracker()

    val tt1 = t1.combine(watcherTransducer(1))
    val tt2 = t2.combine(watcherTransducer(-1))

    val m = IntVectorMonoid[EdgeId]()
    val tt = tt1.product(tt2).mapOut{
      case (Some(e1), Some(e2)) => m.plus(e1, e2)
      case (Some(e1), None) => e1
      case (None, Some(e2)) => e2
      case (None, None) => m.unit
    }.stateAny()

    println(s"watcher.transitions.size=${watcher.transitions.size}")
    println(s"tt.transitions.size=${tt.transitions.size}")


//    tt.outToNFA.saveSVG("before")

    //    val regex = tt.outToNFA.simplifyWithOutIncrease
//    println(tt.fin.size)
//    println(tt.transitions.size)
//    println(s"regex: ${regex.epsFreeTransitions.size}")
//    println(s"regex: ${regex.epsFreeTransitions.map(t=>t.in.size).sum}")
//    println(s"regex: ${tt.outToNFA.toRegex.size}")
//    regex.saveSVG("after")

    val pa = ParikhAutomaton(
      start = tt.start,
      fin = tt.fin,
      transitions = tt.normalTransitions.map(t => Transition(t.from, t.to, t.out.getOrElse(m.unit), t.id))
    )(m)

    val res = pa.solveEdgeUseCount(AndList(Seq(
      pa.chCountPresburgerFormula,
      pa.acceptConstraint,
      GreaterThan(
        Variable(s"sum_y_source_${pa.start}"),
        Constant(0),
      ),
    ) ++ pa.keys.toSeq.map(key =>
      Equal(pa.KeyCountVar(key), Constant(0))
    )))

//    pa.saveSVG("pa", "", res.get)

    println(s"pares: ${res.isDefined}")
    return res.isDefined
  }

  def solveCommonSubstrings(words: Seq[String], alignPrefLen: Int, maxLen: Option[Int]): Option[Seq[Int]] = {
    val ts: Seq[NormalFormTransducer[Any, Char, String]] =
      words.sortBy(w=>w.length).map(s =>
        SubStrCountTransducer(s, alphabets).stateAny()
      )

    val outputEdgeIDs = ts.flatMap(t => t.normalTransitions).filter(t => t.out.isDefined).map(t => t.id)

    println(s"outEE: ${outputEdgeIDs.size}")
    val nfas = ts.map(transducerInToNFA)

    var watcher = nfas.reduce[NFA[Any, Char]]((l,r)=>ParallelNFA(l,r).stateAny()).uniquify

    if(alignPrefLen > 0) {
//      watcher = ParallelNFA(watcher, prefixNFA(alignPrefLen, alphabets).stateAny()).stateAny()
      watcher = addPrefixToNFA(watcher, alignPrefLen, alphabets).stateAny()
    }

//    val parikhWathcer = NormalFormTransducer(
//      start = watcher.start,
//      fin = watcher.fin,
//      normalTransitions = watcher.transitions.map(t => NormalFormTransducerTransition(
//        t.from, t.to, t.in, Some(outputEdgeIDs.map(eid => (eid, if(t.id.contains(eid)) 1 else 0)).filter((e,c)=>c>0).toMap), t.id
//      ))
//    )



//    val pares = solveWithWatcherViaCone(watcher)
    val ret = solveWithWatcher(watcher, maxLen)
    //
//    val ret = solveWithParikhImageWatcher(parikhWathcer, maxLen)
//    assert(pares == ret.isDefined)

    ret
  }

  def solveCommonParikhImage: Option[Seq[Int]] = {
    val (at, bt) = transducers
    val ap = at.parikhAutomaton

    transducerInToNFA(bt).solveInputWord(And(
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

def SubStrCountNFA(word: String, alphabets: Set[Char]): NFA[String,Char] =
  transducerInToNFA(SubStrCountTransducer(word, alphabets))

def SubStrCountTransducer(word: String, alphabets: Set[Char]): NormalFormTransducer[String, Char, String] = {
  NormalFormTransducer(
    start = s"0",
    fin = word.indices.map(idx => s"$idx").toSet,
    normalTransitions = word.indices.flatMap(idx =>
      alphabets.toSeq.map(other => {
        val newWord = word.substring(0, idx) + other
        val jumpTo =
          (0 to newWord.length).findLast(l => word.substring(0, l) == newWord.substring(newWord.length - l, newWord.length)).get % word.length

        NormalFormTransducerTransition(
          from = s"$idx",
          to = s"${jumpTo}",
          in = Some(other),
          out = if(jumpTo <= idx) Some(s"${word}_${idx}_${jumpTo}") else None,
          id = UniqueEdgeId.get)
      })
    )
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

def prefixNFA(len: Int, alphabets: Set[Char]): NFA[String,Char] = {
  val transitions = ListBuffer[Transition[String, Char]]()
  val main = "main"
  def dfs(curState: String, rest: Int): Unit = {
    if (rest == 0) {
      transitions.addAll(alphabets.map(ch =>
        Transition(
          from = curState,
          to = main,
          in = ch,
          id = UniqueEdgeId.get
        )
      ))
    } else {
      alphabets.foreach(ch => {
        val nextState = curState + ch
        transitions.addOne(
          Transition(
            from = curState,
            to = nextState,
            in = ch,
            id = UniqueEdgeId.get
          )
        )
        dfs(nextState, rest - 1)
      })
    }
  }
  val start = "p_"

  dfs(start, len - 1)

  EPSFreeNFA(
    start = start,
    fin = Set(main),
    epsFreeTransitions = transitions.toSeq ++ alphabets.toSeq.map(ch => Transition(from = main, to = main, in = ch, id = UniqueEdgeId.get))
  )
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

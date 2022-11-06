package pcp

import transducer.{NormalFormTransducer, *}
import presburger.*
import regex.{simplifyWithOutIncrease, toRegex}
import dataType.*
import graph.{EdgeUseCountVar, UniqueEdgeId}
import automaton.{EPSFreeNFA, NFA, ParikhAutomaton, Transition, connectivityAwareSolve, solveParikhImageToZeroWithLP}
import solver.ParallelNFA
import util.{EdgeUseCountTracker, *}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map as MutableMap, Set as MutableSet}
import scala.util.Random
case class Tile(u: String, d: String)

case class PCP
(
  val tiles: Seq[Tile]
) {
  System.loadLibrary("jniortools")

  val alphabets = tiles.flatMap(tile=>tile.d++tile.u).toSet
  val transducers =
    (toTransducer(tiles.map(t=>t.u).toList).addPrefix("t1"), toTransducer(tiles.map(t=>t.d).toList).addPrefix("t2"))

  def transduce(word: Seq[Int]): (String, String) = (
      word.foldLeft("")((s, idx) => s + tiles(idx).u),
      word.foldLeft("")((s, idx) => s + tiles(idx).d),
    )

  def shrinkNone[Out](tt: NormalFormTransducer[Any, Int, IntVector[Out]])(implicit tracker: EdgeUseCountTracker = EdgeUseCountTracker()): NormalFormTransducer[Any, Int, IntVector[Out]] = {

    var transitions = tt.normalTransitions

    while(true) {
      tracker.newSession()

      val states = transitions.map(t=>t.from).toSet.intersect(transitions.map(t=>t.to).toSet)
      val fromMap = transitions.groupBy(t=>t.from)
      val toMap = transitions.groupBy(t=>t.to)
      val rTarget = states.filter(s=> fromMap(s).size == 1 && toMap(s).exists(t=>t.in.isDefined) && fromMap(s).exists(t=>t.in.isEmpty))
      if(rTarget.isEmpty)
        return NormalFormTransducer[Any, Int, IntVector[Out]](
          start = tt.start,
          fin = tt.fin,
          normalTransitions = transitions
        )

      val ivm = IntVectorMonoid[Out]()

      transitions = transitions.filterNot(t => rTarget.contains(t.to) || rTarget.contains(t.from)) ++
        rTarget.toSeq.flatMap(target => {
          val going = fromMap(target).head
          toMap(target).map(t => {
            tracker.AddPart(going.id, t.id)

            NormalFormTransducerTransition(
              from = t.from,
              to = going.to,
              in = t.in,
              out = Some(ivm.plus(t.out.get, going.out.get)),
              id = t.id
            )
          })
        })
    }
    assert(false)
  }

  def makeTT[Out](watcher: NormalFormTransducer[Any, Char, IntVector[Out]])(implicit tracker: EdgeUseCountTracker = EdgeUseCountTracker()) = {
    val (rt1, rt2) = transducers
    val (t1, t2) = (
      rt1.normalForm,
      rt2.normalForm
    )

    val tt2 = shrinkNone(t2.combine(watcher).stateAny())
    val tt1 = shrinkNone(t1.combine(watcher).stateAny())

    val m = IntVectorMonoid[Out]()

    val tt: NormalFormTransducer[Any, Int, IntVector[Out]] =
      normalizeTT(
        tt1.product(tt2).mapOut {
          case (Some(e1), Some(e2)) => m.plus(e1, e2.map((key, count) => (key, -count)))
          case (Some(e1), None) => e1
          case (None, Some(e2)) => e2.map((key, count) => (key, -count))
          case (None, None) => m.unit
        }.stateAny()
      )
    tt
  }

  def solveEUCWithParikhImageWatcher[Out](watcher: NormalFormTransducer[Any, Char, IntVector[Out]], integer: Boolean, connectivity: Boolean = true): Option[Map[EdgeId, Double]] = {
    implicit val tracker = EdgeUseCountTracker()

    val tt: NormalFormTransducer[Any, Int, IntVector[Out]] = makeTT(watcher)

    val m = IntVectorMonoid[Out]()

    val pa = ParikhAutomaton(
      start = tt.start,
      fin = tt.fin,
      transitions = tt.normalTransitions.map(t => Transition(t.from, t.to, t.out.getOrElse(m.unit), t.id))
    )(m)

    val reuc = solveParikhImageToZeroWithLP(pa, integer, connectivity)
    reuc match {
      case Some(euc) => {
        val extendedEUC = tracker.calc[Double](euc)
        Some(extendedEUC)
      }
      case None => None
    }
  }

  def normalizeTT[Out](tt: NormalFormTransducer[Any, Int, IntVector[Out]])(implicit tracker: EdgeUseCountTracker = EdgeUseCountTracker()): NormalFormTransducer[Any, Int, IntVector[Out]] = {
    val newFin = "nPA_Fin"
    val newStart = "nPA_start"
    tracker.newSession()

    NormalFormTransducer[Any, Int, IntVector[Out]](
      start = newStart,
      fin = Set(newFin),
      normalTransitions = tt.normalTransitions.filterNot(t=>t.from == tt.start) ++
        tt.sourceFrom(tt.start).flatMap(t => {
          val aid = UniqueEdgeId.get
          val bid = UniqueEdgeId.get
          tracker.AddPart(t.id, aid)
          tracker.AddPart(t.id, bid)

          Seq(
            NormalFormTransducerTransition(
              from = newStart,
              to = t.to,
              in = t.in,
              out = t.out.headOption,
              id = aid
            ),
            NormalFormTransducerTransition(
              from = t.from,
              to = t.to,
              in = t.in,
              out = t.out.headOption,
              id = bid
            )
          )
        }) ++
        tt.fin.map(s =>
          NormalFormTransducerTransition(
            from = s,
            to = newFin,
            in = None,
            out = Some(IntVectorMonoid[Out]().unit),
            id = UniqueEdgeId.get
          )
        )
    )
  }

  def solveWithParikhImageWatcher[Out](watcher: NormalFormTransducer[Any, Char, IntVector[Out]], maxLen: Option[Int]): Option[Seq[Int]] = {
    val m = IntVectorMonoid[Out]()
    val tt: NormalFormTransducer[Any, Int, IntVector[Out]] = makeTT(watcher)

    val pa = ParikhAutomaton(
      start = tt.start,
      fin = tt.fin,
      transitions = tt.normalTransitions.map(t => Transition(t.from, t.to, t.out.getOrElse(m.unit), t.id))
    )(m)


    val yuru = connectivityAwareSolve(pa, true, false)
    if(yuru.isEmpty) {
      println(s"linear relaxed unsat")
      return None
    }
    return Some(Seq())
    println(s"relax ok")

    val lpres = connectivityAwareSolve(pa, true, false)
    if(lpres.isDefined) {
      val trail = tt.eulerTrail(pa.start, lpres.get.map((k,v)=>(k,Math.round(v).toInt))).get.flatMap(t => t.in)

//      val (o1, o2) = transduce(trail)
//      val watcherUseCount = transducerInToNFA(watcher).getUseCount(o1.toList)
//      val watcherUseCount2 = transducerInToNFA(watcher).getUseCount(o2.toList)
//      assert(watcherUseCount == watcherUseCount2)

      return Some(trail)
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

  def solveCommonSubstrings(words: Seq[String], alignPrefLen: Int, maxLen: Option[Int]): Option[Seq[Int]] = {
    val ts: Seq[NormalFormTransducer[Any, Char, String]] =
      words.sortBy(w=>w.length).map(s =>
        SubStrCountTransducer(s, alphabets).stateAny()
      )

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

def toTransducer(ss: List[String]): Transducer[String, Int, List[Char]] =
  Transducer(
    "q", Set("q"), ss.zipWithIndex.map({ case (s, idx) =>
      ListTransducerTransition[String, Int, Char](s"q", "q", Some(idx), s.toList, s"$idx")
    })
  )(ListMonoid[Char]())

//def toTransducer(ss: List[String]): EPSFreeTransducer[String, Int, List[Char]] =
//  EPSFreeTransducer(
//    "0", ss.indices.map(i=>i.toString).toSet, ss.zipWithIndex.flatMap({ case (fromS, fromIdx) =>
//      ss.zipWithIndex.map({ case (toS, toIdx) =>
//        TransducerTransition[String, Int, List[Char]](
//          fromIdx.toString, toIdx.toString, toIdx, toS.toList, s"${fromIdx}_${toIdx}"
//        )
//      })
//    })
//  )(ListMonoid[Char]())

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
          out = if(jumpTo <= idx) Some(s"${word}_${idx}_${other}") else None,//Some(UniqueEdgeId.get),
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

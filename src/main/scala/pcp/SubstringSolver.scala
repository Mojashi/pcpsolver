package pcp

import graph.UniqueEdgeId
import pcp.Instances
import presburger.*
import transducer.*
import automaton.{NFA, Transition}
import dataType.{IntVector, IntVectorMonoid}
import util.*

import java.util
import scala.collection.mutable.{ListBuffer, Set as MutableSet}
import scala.util.Random

object SubstringSolver {
  def refine(pcp: PCP, maxTry: Int): Option[Boolean] = {
    var watcher:NFA[Any, Char] = NFA(
      start = "qq",
      fin = Set("qq"),
      transitions = pcp.alphabets.toSeq.map(ch =>
        Transition(
          from = "qq",
          to = "qq",
          in = Some(ch),
          id = s"qq_${ch}"
        )
      )
    )

    //watcher = SubStrCountNFA("11011", pcp.alphabets).stateAny()

    val pcpEdgeIds = pcp.transducers._1.edges.map(e=>e.id).toSet

    var lastUsedEdges = watcher.edges.map(e=>e.id)

    for (i <- 1 to maxTry) {
      println(s"wacher.transitions.size: ${watcher.transitions.size}")

      watcher.saveSVG[Int]("watcher")
      val twiceUsedCountV = watcher.states.filter(v=>watcher.targetTo(v).map(e=>if(lastUsedEdges.contains(e.id)) 1 else 0).sum > 1)
      println(twiceUsedCountV)

      val nextSplit = watcher.edges.filter(e=>lastUsedEdges.contains(e.id) && twiceUsedCountV.contains(e.to)).map(target => {
        val nextW = watcher.splitE(target.id)
        val pa = NormalFormTransducer[Any, Char, IntVector[EdgeId]](
          start = nextW.start,
          fin = nextW.fin.map(identity),
          normalTransitions = nextW.transitions.map(t =>
            NormalFormTransducerTransition(
              from = t.from,
              to = t.to,
              in = t.in,
              out = Some(Map((t.id, 1))),
              id = t.id
            )
          )
        )

        val ans = pcp.solveEUCWithParikhImageWatcher(pa, true, false)

        if (ans.isEmpty) {
          println("unsolvable")
          return Some(false)
        }

        val euc = ans.get.filter((eid, _) => pcpEdgeIds.contains(eid))
        val sumCost = euc.values.sum
        (sumCost, target, ans.get)
      }).maxBy((sc, _, _)=>sc)

      watcher = watcher.splitE(nextSplit(1).id)
      lastUsedEdges = watcher.transitions.map(e=>e.id).filter(eid=>nextSplit(2).getOrElse(eid,0.0) > 0.001)

//      timer {
//        watcher = refineWatcher(watcher, ans.get.map((k, v) => (k, v)))
//      }
    }
    None
  }


  def main(pcp: PCP, maxTry: Int): Option[Either[Set[String], String]]= {
    var words = MutableSet.from(pcp.alphabets.map(ch => s"$ch"))
    //words = MutableSet.from(pcp.tiles.flatMap(tile1 => pcp.tiles.flatMap(tile2 => Seq(tile1.u + tile2.u, tile1.d + tile2.d))).toSet)

    val alignPrefLen = 0
    for(i <- 1 to maxTry) {
      if(words.toSeq.map(_.length).sum >= 30) {
        words = MutableSet(Random.shuffle(MutableSet.from(enumerateStr(3, pcp.alphabets.toSeq)).toSeq).head)
      }

      val ans = pcp.solveCommonSubstrings(words.toSeq, alignPrefLen, None)
      println(s"words: $words, alignPrefLen: $alignPrefLen")
//      println(ans)
//      println(ans.flatMap{a=>Some(a.groupBy(v=>v).mapValues(s=>s.size).toMap)})

      if (ans.isEmpty) {
        println(s"unsolvable ${words}")
        return Some(Left(words.toSet))
      }
      val (o1, o2) = pcp.transduce(ans.get)
//      println(o1)
//      println(o2)

//      if (o1 == o2) {
//        println(s"ans: ${ans}")
//        return Some(Right(ans.get.mkString))
//      }
      val diff = findDiffSubstr(o1.substring(alignPrefLen), o2.substring(alignPrefLen))
      println(s"diff: $diff")
//      if (!words.forall(w => o1.substring(alignPrefLen).countSubstring(w) == o2.substring(alignPrefLen).countSubstring(w)))
//        assert(false)
      words.addOne(diff)
    }
    None
  }

  def dictbase(pcp: PCP): Boolean = {
    val dict = pcp.tiles.flatMap(tile1 => pcp.tiles.flatMap(tile2 => Seq(tile1.u + tile2.u, tile1.d + tile2.d))).toSet.toSeq
    //enumerateStr(5, pcp.alphabets.toSeq) ++ enumerateStr(6, pcp.alphabets.toSeq)

    val alignPrefLen = 2
    for(i <- 1 until 40) {
      var words = dict
//        (
//          (0 until 8).map(_=>dict(Random.nextInt(dict.size))) ++
//          enumerateStr(2, pcp.alphabets.toSeq) ++
//          enumerateStr(3, pcp.alphabets.toSeq)
//        ).toSet.toSeq

//      words = Seq("001", "100", "110", "11", "001110", "010", "001001", "01001", "10011", "011", "000", "100010", "00", "101", "10", "01100", "111", "000010", "11000", "01")

      val ans = pcp.solveCommonSubstrings(words, alignPrefLen, None)
      println(s"words: $words, alignPrefLen: $alignPrefLen")
      println(ans)
      println(ans.flatMap { a => Some(a.groupBy(v => v).mapValues(s => s.size).toMap) })

      if (ans.isEmpty) {
        println(s"unsolvable ${words}")
        return true
      }

     // val (o1, o2) = pcp.transduce(ans.get)
//      println(o1)
//      println(o2)

//      if (o1 == o2) {
//        println(s"ans: ${ans}")
//        return true
//      }
//      if (!words.forall(w => o1.countSubstring(w) == o2.countSubstring(w)))
//        assert(false)
    }
    return false
  }
}

def enumerateStr(length: Int, alphabet: Seq[Char]): Seq[String] =
  if(length==0) Seq("")
  else enumerateStr(length - 1, alphabet).flatMap(s => alphabet.map(ch=>s + ch))

def findDiffSubstr(a: String, b: String): String = {

  val alphabets = a.toSet.intersect(b.toSet)

  def find(cur: String, len: Int): Option[String] = {
    if (cur.length == len)
      if (a.countSubstring(cur) != b.countSubstring(cur))
        Some(cur)
      else None
    else {
      for (ch <- alphabets) {
        val c = find(cur + ch, len)
        if (c.isDefined) return c
      }
      None
    }
  }

  for(len <- 1 to 100) {
    val r = find("", len)
    if(r.isDefined) return r.get
  }
  println(a)
  println(b)
  assert(false)
}


extension (s: String) {
  def countSubstring(substr: String):Int = {
    s"$substr".r.findAllIn(s).length
  }
}

extension[State, Alphabet] (nfa: NFA[State, Alphabet]) {
  def splitE(eid: EdgeId): NFA[Any, Alphabet] = {
    val e = nfa.idToedgesMap(eid)
    val newV = ("split", eid)

    NFA[Any, Alphabet] (
      start = nfa.start,
      fin = nfa.fin ++ (if(nfa.fin.contains(e.to)) Set(newV) else Set()),
      transitions = nfa.stateAny().transitions.filter(e=>e.id != eid) ++ Seq(
        Transition(
          from = e.from,
          to = newV,
          in = e.in,
          id = (newV, "in").toString()
        )) ++
      nfa.sourceFrom(e.to).map(o => Transition(
        from = newV,
        to = o.to,
        in = o.in,
        id = (newV, "out", o.in).toString()
      ))
    )
  }
}
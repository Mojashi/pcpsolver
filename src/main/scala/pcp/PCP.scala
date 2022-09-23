package pcp

import transducer.*
import presburger.*

import scala.collection.mutable
case class Tile(u: String, d: String)

class PCP
(
  tiles: List[Tile]
) {
  val alphabets = tiles.flatMap(tile=>tile.d++tile.u).toSet
  def transducers: (Transducer[List[Char]],Transducer[List[Char]]) =
    (toTransducer(tiles.map(t=>t.u)), toTransducer(tiles.map(t=>t.d)))

  def transduce(word: Seq[Int]): (String, String) = {
    val (at, bt) = transducers
    (
      word.foldLeft("")((s, idx) => s + tiles(idx).u),
      word.foldLeft("")((s, idx) => s + tiles(idx).d),
    )
  }

//  def solveCommonSubstrParikhImage(targets: List[String]): Option[List[Int]] = {
//
//    targets.map(target => target.)
//  }
  def solve: Seq[Int] = {
    val words = mutable.HashSet[String]()

    while(true) {
      val ans = commonSubstringImage(words.toSet).get

      val (r1, r2) = transduce(ans)
      println(ans)
      println(r1)
      println(r2)
      if(r1 == r2) {
        return ans
      }

      var minDiff: Option[String] = None
      for(i <- 1 to 10) {
        if (minDiff.isEmpty) {
          def find(cur: String): Option[String] = {
            if (cur.length == i) {
              if (r1.countSubstring(cur) != r2.countSubstring(cur))
                return Some(cur)
            } else {
              for (ch <- alphabets) {
                val v = find(cur + ch)
                if (v.isDefined) return v
              }
            }
            None
          }

          minDiff = find("")
        }
      }

      println(s"differ: $minDiff")
      words.add(minDiff.get)
    }
    Seq()
  }

//  def eulerTrail(start: State, useCount: Map[EdgeId, Int]): Seq[E] = {
//    val used = MutableMap[EdgeId, Int]()
//    val trail = ArrayBuffer[E]()
//
//    def dfs(cur: V, fromEdge: Option[E], s1:StringBuffer, s2:StringBuffer): Unit = {
//      edgesMap.getOrElse(cur, List()).foreach(edge => {
//        if (used.getOrElseUpdate(edge.id, 0) < useCount.getOrElse(edge.id, 0)) {
//          used(edge.id) += 1
//          dfs(edge.to, Some(edge, )
//        }
//      })
//      fromEdge match
//        case Some(e) => trail.append(e)
//        case None =>
//    }
//
//    dfs(start, None)
//
//    trail.reverse.toSeq
//  }
//

  def commonSubstringImage(ss: Set[String]): Option[Seq[Int]] = {
    var (at, bt) = transducers

    ss.foreach( s => {
      var willExpand = (at.findWrappedTrans(s) ++ bt.findWrappedTrans(s)).toSet
      while(!willExpand.isEmpty) {
        willExpand.foreach(t => {
          at = at.expandTransition(t)
          bt = bt.expandTransition(t)
        })

        willExpand = (at.findWrappedTrans(s) ++ bt.findWrappedTrans(s)).toSet
      }
    })

    val (avt, bvt) = (VertexOrientedTransducer(at), VertexOrientedTransducer(bt))
    println(avt.states.size)

    bvt.solve(
      AndList(Seq(
        GreaterThan(
          alphabets.map(ch => avt.parikhAutomaton.KeyCountVar(ch)).reduce((l, r) => Add(l, r)),
          Constant(0)
        ),
        GreaterThan(
          Constant(20),
          alphabets.map(ch => avt.parikhAutomaton.KeyCountVar(ch)).reduce((l, r) => Add(l, r)),
        ),
        avt.presburgerFormulaWithSubstrCount(ss)
      )),
      ss
    ).flatMap(s=>Some(s.map(ch=>ch.toString.toInt)))
  }
  def solveCommonParikhImage: Option[Seq[Int]] = {
    val (at, bt) = transducers
    val ap = at.parikhAutomaton

    bt.solve(And(
      ap.chCountPresburgerFormula,
      // forbid empty
      GreaterThan(
        alphabets.map(ch => ap.KeyCountVar(ch)).reduce((l,r)=>Add(l,r)),
        Constant(0)
      )
    )).flatMap(s=>Some(s.map(ch=>ch.toString.toInt)))
  }
}

def toTransducer(ss: List[String]): Transducer[List[Char]] =
  Transducer(
    "q", Set("q"), ss.zipWithIndex.map({ case (s, idx) =>
      Transition("q", "q", idx.toString, s.toList, idx.toString)
    })
  )(ListMonoid[Char]())

package pcp

import graph.UniqueEdgeId
import pcp.Instances
import presburger.*
import transducer.*
import util.*

import java.util
import scala.collection.mutable.{ListBuffer, Set as MutableSet}
import scala.util.Random

object SubstringSolver {
//  val pcp = Instances.pcp43Unsolved2

  def main(pcp: PCP): Option[Either[Set[String], String]]= {
    var words = MutableSet.from(pcp.alphabets.map(ch => s"$ch"))

    val alignPrefLen = 0
    for(i <- 1 to 30) {
      if(words.toSeq.map(_.length).sum >= 25) {
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
    val dict = enumerateStr(5, pcp.alphabets.toSeq) ++ enumerateStr(6, pcp.alphabets.toSeq)

    val alignPrefLen = 0
    for(i <- 1 until 100) {
      val words =
        (
          (0 until 8).map(_=>dict(Random.nextInt(dict.size))) ++
          enumerateStr(2, pcp.alphabets.toSeq) ++
          enumerateStr(3, pcp.alphabets.toSeq)
        ).toSet.toSeq

      val ans = pcp.solveCommonSubstrings(words, alignPrefLen, None)
      println(s"words: $words, alignPrefLen: $alignPrefLen")
      println(ans)
      println(ans.flatMap { a => Some(a.groupBy(v => v).mapValues(s => s.size).toMap) })

      if (ans.isEmpty) {
        println(s"unsolvable ${words}")
        return false
      }

      val (o1, o2) = pcp.transduce(ans.get)
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
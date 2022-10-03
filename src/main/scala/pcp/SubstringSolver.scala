package pcp

import graph.UniqueEdgeId
import pcp.Instances
import presburger.*
import transducer.*
import util.*

import java.util
import scala.collection.mutable.ListBuffer

object SubstringSolver extends App {
  main()


  def main(): Unit = {
    val pcp = Instances.pcpUnsolved5

    var words = ListBuffer.from(pcp.alphabets.map(ch => s"$ch"))

    words = ListBuffer("111", "10")

    val alignPrefLen = 0
    while (true) {

      val ans = pcp.solveCommonSubstrings(words.toSeq, alignPrefLen, None)
      println(s"words: $words, alignPrefLen: $alignPrefLen")
      println(ans)
      println(ans.flatMap{a=>Some(a.groupBy(v=>v).mapValues(s=>s.size).toMap)})
      val (o1, o2) = pcp.transduce(ans.get)
      println(o1)
      println(o2)

      if (o1 == o2) return

      val diff = findDiffSubstr(o1.substring(alignPrefLen), o2.substring(alignPrefLen))
      println(s"diff: $diff")
      if (words.contains(diff)) assert(false)
      words.addOne(diff)
    }
  }
}

def findDiffSubstr(a: String, b: String): String = {
  val alphabets = a.toSet.intersect(b.toSet)

  def find(cur: String, len: Int): Option[String] = {
    if(cur.length == len)
      if(a.countSubstring(cur) != b.countSubstring(cur))
        Some(cur)
      else None
    else {
      for(ch <- alphabets) {
        val c = find(cur + ch, len)
        if(c.isDefined) return c
      }
      None
    }
  }

  for(len <- 1 to 100) {
    val r = find("", len)
    if(r.isDefined) return r.get
  }
  assert(false)
}


extension (s: String) {
  def countSubstring(substr: String):Int = {
    s"$substr".r.findAllIn(s).length
  }
}
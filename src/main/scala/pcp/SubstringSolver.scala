package pcp

import graph.UniqueEdgeId
import presburger.*
import transducer.*
import util.*

import java.util
import scala.collection.mutable.ListBuffer

object SubstringSolver extends App {
  main()


  def main(): Unit = {

    val pcp1 = PCP(List(
      Tile("100", "1"),
      Tile("0", "100"),
      Tile("1", "00"),
    ))
    val pcp2 = PCP(List(
      Tile("11011", "1"),
      Tile("10", "1011"),
      Tile("1", "10"),
    ))
    val pcp3 = PCP(List(
      Tile("110", "00"),
      Tile("011", "1"),
      Tile("00", "1"),
      Tile("1", "100"),
    ))
    val pcpUnsolved = PCP(List(
      Tile("111", "110"),
      Tile("111", "101"),
      Tile("000", "00"),
      Tile("1", "111"),
    ))


    val pcpUnsolved2 = PCP(List(
      Tile("111", "110"),
      Tile("111", "101"),
      Tile("000", "0"),
      Tile("11", "111"),
    ))


    //111  111  10   0
    //110  1    111  000
    val pcpUnsolved3 = PCP(List(
      Tile("111", "110"),
      Tile("111", "1"),
      Tile("10", "111"),
      Tile("0", "000"),
    ))

    //101  00   1    0
    //1    0    101  1
    val pcpUnsolved4 = PCP(List(
      Tile("101", "1"),
      Tile("00", "0"),
      Tile("1", "101"),
      Tile("0", "1"),
      Tile("1", "11"),
    ))

    val pcp = pcpUnsolved4

    val words = ListBuffer.from(pcp.alphabets.map(ch => s"$ch"))

    while (true) {
      val ans = pcp.solveCommonSubstrings(words.toSeq, None)
      println(ans)
      println(ans.flatMap{a=>Some(a.groupBy(v=>v).mapValues(s=>s.size).toMap)})
      val (o1, o2) = pcp.transduce(ans.get)
      println(o1)
      println(o2)

      if (o1 == o2) return

      val diff = findDiffSubstr(o1, o2)
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
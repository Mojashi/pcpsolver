package regex

import org.scalatest.funsuite.AnyFunSuite
import pcp.*
import flatAutomaton.FlatAutomaton
class RegexTest extends AnyFunSuite {

  val pcp = PCP(List(
    Tile("abb", "a"),
    Tile("b", "abb"),
    Tile("a", "bb"),
  ))

  val inFa = FlatAutomaton[Int]("in", 2, 2, pcp.tiles.indices.toSet)

  println(
    inFa.toRegex
  )
  println(inFa.toRegex.size)
}

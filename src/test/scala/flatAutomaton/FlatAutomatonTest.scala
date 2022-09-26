package flatAutomaton

import org.scalatest.funsuite.AnyFunSuite
import pcp.*
import solver.{ParalellTransducerNFA, ParallelNFA}
import transducer.normalForm
import presburger.*
import util.{PrettyPrintMap, EdgeUseCountTracker}
class FlatAutomatonTest extends AnyFunSuite {
  test("svg check") {
    val fa = FlatAutomaton("a", 3, 5, Set('0','1'))

    fa.saveSVG("fa")
  }

  test("pcp transduce") {
    val inFa = FlatAutomaton[Int]("in", 3, 3, Set(0, 1, 2, 3))
    val outFa = FlatAutomaton[Char]("out", 3, 3, Set('0', '1'))

    val pcp = PCP(List(
      Tile("111", "010"),
      Tile("111", "1"),
      Tile("10", "100"),
      Tile("0", "111")
    ))

    implicit val tracker = EdgeUseCountTracker()
    val trans = pcp.transducers._1.normalForm
    val p = ParalellTransducerNFA(inFa, trans, outFa)
    println("svg")
//    p.saveSVG("pnfa")
    println("finsvg")

    val ans = p.solveInputWord(AndList(List(
      inFa.purityConstraint,
      outFa.purityConstraint,
      p.acceptConstraint,
      outFa.parikhAutomaton.chCountPresburgerFormula,
      tracker.formula,
      GreaterThan(Variable("alphabet_0"), Constant(5)),
      GreaterThan(Variable("alphabet_1"), Constant(5))
    )))

//    println(tracker.parts.toMap.prettyPrint)

    println(ans)
  }

  test("pcp solve") {
    val p = 1
    val q = 7
    val pcp = PCP(List(
      Tile("100", "1"),
      Tile("0", "100"),
      Tile("1", "00"),
    ))

    val inFa = FlatAutomaton[Int]("in", p, q, pcp.tiles.indices.toSet)
    val out1Fa = FlatAutomaton[Char]("out1", p, q, Set('0', '1'))
    val out2Fa = FlatAutomaton[Char]("out2", p, q, Set('0', '1'))

    inFa.saveSVG("inFa")

    val trans1 = pcp.transducers._1.addPrefix("t1").normalForm
    val trans2 = pcp.transducers._2.addPrefix("t2").normalForm
    val tracker1 = EdgeUseCountTracker()
    val p1 = ParalellTransducerNFA(inFa, trans1, out1Fa)(tracker1)
    val tracker2 = EdgeUseCountTracker()
    val p2 = ParalellTransducerNFA(inFa, trans2, out2Fa)(tracker2)
    val tracker3 = EdgeUseCountTracker()
    val equalNFA = ParallelNFA(out1Fa, out2Fa)(tracker3)

    val ans = inFa.solveInputWord(AndList(List(
      inFa.purityConstraint,
      out1Fa.purityConstraint,
      out2Fa.purityConstraint,
      equalNFA.acceptConstraint,
      p1.acceptConstraint,
      p2.acceptConstraint,
      inFa.parikhAutomaton.chCountPresburgerFormula,
      tracker1.formula,
      tracker2.formula,
      tracker3.formula,
      GreaterThan(Variable("y_flat_in(0,0,Some(0))"), Constant(0)),
    )))

    equalNFA.saveSVG("equalNFA")

//        println(tracker.parts.toMap.prettyPrint)
//    println(inFa.purityConstraint.prettyPrint())

    println(ans)
    println(pcp.transduce(ans.get))
  }
}

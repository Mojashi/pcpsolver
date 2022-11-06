package flatAutomaton

import org.scalatest.funsuite.AnyFunSuite
import pcp.*
import solver.{ParalellTransducerNFA, ParallelNFA}
import transducer.normalForm
import presburger.*
import util.{PrettyPrintMap, EdgeUseCountTracker}
import automaton.ConstantAutomaton
class FlatAutomatonTest extends AnyFunSuite {
  test("svg check") {
    val fa = FlatAutomaton("a", 3, 5, Set('0','1'))

    fa.saveSVG[Int]("fa")
  }


  test("pcp solve") {
//    val p = 1
//    val q = 7
    val pcp = PCP(List(
      Tile("abb", "a"),
      Tile("b", "abb"),
      Tile("a", "bb"),
    ))

    val inFa = FlatAutomaton[Int]("in", 1, 7, pcp.tiles.indices.toSet)
    val out1Fa = FlatAutomaton[Char]("out1", 4, 2, pcp.alphabets)
    val out2Fa = FlatAutomaton[Char]("out2", 4, 2, pcp.alphabets)

    inFa.saveSVG[Int]("inFa")

    val trans1 = pcp.transducers._1.addPrefix("t1").normalForm
    val trans2 = pcp.transducers._2.addPrefix("t2").normalForm
    val tracker1 = EdgeUseCountTracker()
    val p1 = ParalellTransducerNFA(inFa, trans1, out1Fa)(tracker1)
    val tracker2 = EdgeUseCountTracker()
    val p2 = ParalellTransducerNFA(inFa, trans2, out2Fa)(tracker2)
    val tracker3 = EdgeUseCountTracker()
    val equalNFA = ParallelNFA(out1Fa, out2Fa)(tracker3)

    var formula = AndList(List(
      inFa.purityConstraint,
      out1Fa.purityConstraint,
      out2Fa.purityConstraint,
      equalNFA.acceptConstraint,
      p1.acceptConstraint,
      p2.acceptConstraint,
      inFa.parikhAutomaton.chCountPresburgerFormula,
      out1Fa.parikhAutomaton.chCountPresburgerFormula,
      out2Fa.parikhAutomaton.chCountPresburgerFormula
    ))

    val ts = List(tracker1, tracker2, tracker3)
    for(t <- ts) {
      formula = And(formula , t.formula(formula.enumerateVar))
    }

    val ans = inFa.solveInputWord(AndList(List(
      formula,
      GreaterThan(Variable("y_flat_in(0,0,Some(0))"), Constant(0)),
    )))

//    equalNFA.saveSVG("equalNFA")

//        println(tracker.parts.toMap.prettyPrint)
//    println(inFa.purityConstraint.prettyPrint())

    println(ans)
    println(pcp.transduce(ans.get))
  }

  test("pcp solve 2") {
    //    val q = 7
    val pcp = PCP(List(
      Tile("abb", "a"),
      Tile("b", "abb"),
      Tile("a", "bb"),
    ))

    val inFa = FlatAutomaton[Int]("in", 1, 7, pcp.tiles.indices.toSet)
    val out1Fa = FlatAutomaton[Char]("out1", 4, 2, pcp.alphabets)


    val trans1 = pcp.transducers._1.addPrefix("t1").normalForm
    val trans2 = pcp.transducers._2.addPrefix("t2").normalForm
    val tracker1 = EdgeUseCountTracker()
    val p1 = ParalellTransducerNFA(inFa, trans1, out1Fa)(tracker1)
    val tracker2 = EdgeUseCountTracker()
    val p2 = ParalellTransducerNFA(inFa, trans2, out1Fa)(tracker2)

    var formula = AndList(List(
      inFa.purityConstraint,
      out1Fa.purityConstraint,
      p1.acceptConstraint,
      p2.acceptConstraint,
    ))

    val ts = List(tracker1, tracker2)
    for (t <- ts) {
      formula = And(formula, t.formula(formula.enumerateVar))
    }

    val ans = inFa.solveInputWord(AndList(List(
      formula,
      GreaterThan(Variable("y_flat_in(0,0,Some(0))"), Constant(0)),
    )))

    //    equalNFA.saveSVG("equalNFA")

    //        println(tracker.parts.toMap.prettyPrint)
    //    println(inFa.purityConstraint.prettyPrint())

    println(ans)
    println(pcp.transduce(ans.get))
  }
  test("constant input test") {
    val p = 1
    val q = 4
    val pcp = PCP(List(
      Tile("abb", "ab"),
      Tile("a", "ba"),
    ))

    val inFa = ConstantAutomaton("01string", Seq(0, 1))
    val out1Fa = FlatAutomaton[Char]("out1", p, q, pcp.alphabets)


    val trans1 = pcp.transducers._1.addPrefix("t1").normalForm
    val tracker1 = EdgeUseCountTracker()
    val p1 = ParalellTransducerNFA(inFa, trans1, out1Fa)(tracker1)

    val formula = AndList(List(
      out1Fa.purityConstraint,
      p1.acceptConstraint,
      inFa.parikhAutomaton.chCountPresburgerFormula,
    ))
    val ans = inFa.solveInputWord(AndList(List(
      tracker1.formula(formula.enumerateVar),
      formula
    )))
//
//    p1.saveSVG("p1","",uc.get)

//    println(tracker1.parts.toMap.prettyPrint)
    //    println(inFa.purityConstraint.prettyPrint())

    println(ans)
    println(pcp.transduce(ans.get))
  }
}

package transducer

import dataType.*
import graph.UniqueEdgeId
import org.scalatest.funsuite.AnyFunSuite
import presburger.*
import util.EdgeUseCountTracker

import scala.collection.immutable.Set

class TransducerTest extends AnyFunSuite {
  val t: Transducer[String, Char, List[Char]] = EPSFreeTransducer(
    "0", Set("f"), Seq(
      TransducerTransition("0", "0", 'a', "asada".toList, "1"),
      TransducerTransition("0", "0", 'b', "dddd".toList, "2"),
      TransducerTransition("0", "1", '[', List('['), "3"),
      TransducerTransition("1", "1", 'a', "c".toList, "4"),
      TransducerTransition("1", "1", 'b', List('c'), "5"),
      TransducerTransition("1", "0", ']', List(']'), "6"),
      TransducerTransition("0", "f", '#', List(), "7"),
    )
  )(ListMonoid[Char]())

  test("testSolve") {
    val nf = t.normalForm
    print(transducerInToNFA(nf).solveInputWord(transducerInToNFA(nf).acceptConstraint))
  }

  test("mergeTest") {

    val t2: Transducer[String, Char, List[Char]] = EPSFreeTransducer(
      "0", Set("0"), Seq(
        TransducerTransition("0", "0", 'a', "aa".toList, UniqueEdgeId.get),
        TransducerTransition("0", "0", ']', "cc".toList, UniqueEdgeId.get),
        TransducerTransition("0", "0", '[', "d".toList, UniqueEdgeId.get),
        TransducerTransition("0", "0", 'b', "dd".toList, UniqueEdgeId.get),
        TransducerTransition("0", "0", 'd', "ee".toList, UniqueEdgeId.get),
        TransducerTransition("0", "0", 's', "ss".toList, UniqueEdgeId.get),
      )
    )(ListMonoid[Char]())

    val tnf = t.normalForm
    val t2nf = t2.normalForm
    implicit val tracker = EdgeUseCountTracker()

    val nf = tnf.combine(t2nf)
    nf.saveSVG("nf")
    tnf.saveSVG("t")
    t2nf.saveSVG("t2")
    val pa = nf.parikhAutomaton
    println(tracker)
    print(transducerInToNFA(nf).solveInputWord(
      AndList(List(
        AndList(List(transducerInToNFA(nf).acceptConstraint)),
        pa.chCountPresburgerFormula,
        GreaterThan(pa.KeyCountVar('a'), Constant(1)),
//        tracker.formula
      ))
    ))
  }
}

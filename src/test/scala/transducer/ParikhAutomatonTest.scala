package transducer

import org.scalatest.funsuite.AnyFunSuite
import presburger.*
import dataType.*

class ParikhAutomatonTest extends AnyFunSuite {

  test("testSolve") {
    implicit val m = ListMonoid[Char]()
    val t: Transducer[String, Char, List[Char]] = EPSFreeTransducer(
      "0", Set("f"), Seq(
        TransducerTransition("0", "0", 'a', List('a'), "1"),
        TransducerTransition("0", "0", 'b', List('b'), "2"),
        TransducerTransition("0", "1", '[', List(), "3"),
        TransducerTransition("1", "1", 'a', List('c'), "4"),
        TransducerTransition("1", "1", 'b', List('c'), "5"),
        TransducerTransition("1", "0", ']', List(), "6"),
        TransducerTransition("0", "f", '#', List(), "7"),
      )
    )

    val pa = t.parikhAutomaton
    print(t.solveInputWord(
      AndList(List(
        Equal(
          pa.KeyCountVar('a'),
          Add(
            pa.KeyCountVar('b'),
            Constant(4)
          )
        ),
        GreaterThan(
          pa.KeyCountVar('b'),
          Constant(2),
        ),
        Equal(
          pa.KeyCountVar('c'),
          pa.KeyCountVar('a'),
        ),
      ))
    ))
  }

}

package transducer

import org.scalatest.funsuite.AnyFunSuite
import presburger.*

class ParikhAutomatonTest extends AnyFunSuite {

  test("testSolve") {
    val t: Transducer[List[Char]] = Transducer(
      "0", Set("f"), Seq(
        Transition("0", "0", "a", List('a'), "1"),
        Transition("0", "0", "b", List('b'), "2"),
        Transition("0", "1", "[", List(), "3"),
        Transition("1", "1", "a", List('c'), "4"),
        Transition("1", "1", "b", List('c'), "5"),
        Transition("1", "0", "]", List(), "6"),
        Transition("0", "f", "#", List(), "7"),
      )
    )(ListMonoid[Char]())

    val pa = t.parikhAutomaton
    print(t.solve(
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

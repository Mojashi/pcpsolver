package automaton

import org.scalatest.funsuite.AnyFunSuite

class NFATest extends AnyFunSuite {
  val A1 = NFA[String, Char](
    "q0", Set("qf"),
    Seq(
      Transition("q0", "q1", Some('0'), "f0"),
      Transition("q1", "q1", Some('2'), "f1"),
      Transition("q1", "qf", Some('1'), "f2"),
    )
  )
  val A2 = NFA[String, Char](
    "q0", Set("qf"),
    Seq(
      Transition("q0", "q0", Some('0'), "e0"),
      Transition("q0", "q1", Some('0'), "e1"),
      Transition("q1", "q2", Some('2'), "e2"),
      Transition("q2", "qf", Some('1'), "e3"),
    )
  )
  test("testIntersect") {
    val B = ParallelNFA(A1, A2)
    println(B.solveInputWord(B.acceptConstraint))
  }

}

package pcp

import org.scalatest.funsuite.AnyFunSuite

class SolveTest extends AnyFunSuite {

  val unsol43 = "PCP[4,3]Unsolved"
  val unsol34 = "34_unsol"
  val solved = "200HardInstances"

  test("") {
    val name = unsol43
    val pcps = InstancesParser(s"${name}.txt").instances
    val pcp = pcps(24)
    println(pcp)
    val ans = Solve(pcp).DFS(Seq(), Map())
    println(ans)
  }

}

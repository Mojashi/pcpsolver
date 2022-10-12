package ORTools

import com.google.ortools.linearsolver.*
import org.scalatest.funsuite.AnyFunSuite
import pcp.{PCP, Instances}

class ORTools extends AnyFunSuite {

  System.loadLibrary("jniortools")

  test("PCP") {
    val pcp = Instances.pcp1

//    pcp.solveWithParikhImageWatcher()
  }

  test("ORTools test") {
    val solver = MPSolver.createSolver("SCIP")
    val infinity = java.lang.Double.POSITIVE_INFINITY

    val cons = solver.makeConstraint(0, infinity)
    val x = solver.makeIntVar(1,3, "x")
    val y = solver.makeIntVar(0, 0, "y")
    cons.setCoefficient(x, -1)
    cons.setCoefficient(y, 1000)


    println(solver.solve())
    println(x.solutionValue())
    println(y.solutionValue())
  }

}

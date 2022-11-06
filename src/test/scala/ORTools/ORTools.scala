package ORTools

import com.google.ortools.linearsolver.*
import org.scalatest.funsuite.AnyFunSuite
import pcp.{PCP, Instances}
import ilog.concert.IloException
import ilog.concert.IloLinearNumExpr
import ilog.concert.IloNumVar
import ilog.cplex.IloCplex

class CPLEX extends AnyFunSuite {
  test("CPLEX") {

      val cplex = IloCplex()

      // create model and solve it
      val x = cplex.numVar(0, Double.MaxValue, "x")
      val y = cplex.numVar(0, Double.MaxValue, "y")
      val objective = cplex.linearNumExpr
      objective.addTerm(0.12, x)
      objective.addTerm(0.15, y)

      cplex.addMinimize(objective)

      cplex.addGe(cplex.sum(cplex.prod(60, x), cplex.prod(60, y)), 300)
      cplex.addGe(cplex.sum(cplex.prod(12, x), cplex.prod(6, y)), 36)
      cplex.addGe(cplex.sum(cplex.prod(10, x), cplex.prod(30, y)), 90)

      if (cplex.solve) {
        System.out.println("obj = " + cplex.getObjValue)
        System.out.println("x = " + cplex.getValue(x))
        System.out.println("y = " + cplex.getValue(y))
        cplex.getSolnPoolNsolns
      }
      else System.out.println("Can not solve")
    }
}

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
    
    cons.delete()


    println(solver.solve())
    println(x.solutionValue())
    println(y.solutionValue())
  }

}

import io.github.cvc5.Kind._
import io.github.cvc5._

class CVC5T extends AnyFunSuite {
  test("cvc5") {
    val slv = Solver()

    slv.setLogic("QF_SLIA");
    // Produce models
    slv.setOption("produce-models", "true");
    // The option strings-exp is needed
    slv.setOption("strings-exp", "true");
    // Set output language to SMTLIB2
    slv.setOption("output-language", "smt2");
    slv.mkString("sss")
  }
}
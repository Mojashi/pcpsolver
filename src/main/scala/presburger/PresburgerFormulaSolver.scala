package presburger
import com.microsoft.z3.{BoolExpr, Context, IntExpr, FPNum, IntNum, Params, Status}
import io.github.cvc5.{Solver as CVCSolver}
import scala.collection.mutable.Map as MutableMap
import util.timer


class PresburgerFormulaSolver {

  def solveWithCVC5(formula: ExistentialPresburgerFormula): Option[VarValueMap[Int]] = {
    val solver = CVCSolver()
    solver.setLogic("QF_LRA")
    solver.setOption("produce-models", "true")

    implicit val ctx = CVCContext(solver, MutableMap())
    val cvcFormula = formula.cvcExpr
    solver.assertFormula(cvcFormula)

    if (solver.checkSat().isSat) {
      //      println("sat")
      Some(formula.enumerateVar.map(v =>
        (v, solver.getValue(ctx.variableRegistry.get(v).get).getIntegerValue.intValue())
      ).toMap)
    }
    else {
      println("unsat")
      None
    }
  }
  def solveWithZ3(formula: ExistentialPresburgerFormula): Option[VarValueMap[Int]] = {
    implicit val ctx: Context = new Context()
    val solver = ctx.mkSolver()
    val p = ctx.mkParams
//    p.add("logic", "BV")
    p.add("threads", 8)
    solver.setParameters(p)

    solver.add(formula.z3Expr)
//    println("start solve")

    timer {
      if (solver.check() == Status.SATISFIABLE) {
        //      println("sat")
        Some(formula.enumerateVar.map(v => {
          (v, solver.getModel.evaluate(ctx.mkIntConst(v), true).asInstanceOf[IntNum].getInt)
        }).toMap)
      }
      else {
        println("unsat")
        None
      }
    }
  }

  def findUnSatCore(formula: ExistentialPresburgerFormula): Option[ExistentialPresburgerFormula] = {
    if(solveWithZ3(formula).isDefined)
      None
    else
      formula match {
        case And(left, right) =>
          val lCore = findUnSatCore(left)
          if(lCore.isDefined)
            lCore
          else{
            val rCore = findUnSatCore(right)
            if(rCore.isDefined)
              rCore
            else
              Some(formula)
          }
        case _ => Some(formula)
      }
  }
}
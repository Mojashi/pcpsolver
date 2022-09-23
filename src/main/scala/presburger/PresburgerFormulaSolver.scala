package presburger
import com.microsoft.z3.{BoolExpr, Context, IntExpr, IntNum, Params, Status}

class PresburgerFormulaSolver {
  def solve(formula: ExistentialPresburgerFormula): Option[VarValueMap] = {
    implicit val ctx: Context = new Context()
    val solver = ctx.mkSolver()
    val p = ctx.mkParams
//    p.add("logic", "QF_NRA")
    solver.setParameters(p)

    solver.add(formula.z3Expr)
    if(solver.check() == Status.SATISFIABLE)
      Some(formula.enumerateVar.map( v =>
        (v, solver.getModel.evaluate(ctx.mkIntConst(v), true).asInstanceOf[IntNum].getInt)
      ).toMap)
    else {
      None
    }
  }

  def findUnSatCore(formula: ExistentialPresburgerFormula): Option[ExistentialPresburgerFormula] = {
    if(solve(formula).isDefined)
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
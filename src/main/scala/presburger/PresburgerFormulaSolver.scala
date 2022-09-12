package presburger
import com.microsoft.z3.{BoolExpr, Context, IntExpr, IntNum, Status}

class PresburgerFormulaSolver {
  def solve(formula: ExistentialPresburgerFormula): Option[VarValueMap] = {
    implicit val ctx: Context = new Context()
    val solver = ctx.mkSolver()

    solver.add(formula.z3Expr)
    if(solver.check() == Status.SATISFIABLE)
      Some(formula.enumerateVar.map( v =>
        (v, solver.getModel.evaluate(ctx.mkIntConst(v), true).asInstanceOf[IntNum].getInt)
      ).toMap)
    else None
  }
}
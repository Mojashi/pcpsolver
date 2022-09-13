package presburger
import com.microsoft.z3.{BoolExpr, Context, IntExpr, IntNum}

class PresburgerFormulaSolver {
  def solve(formula: ExistentialPresburgerFormula): VarValueMap = {
    implicit val ctx: Context = new Context()
    val solver = ctx.mkSolver()

    solver.add(formula.z3Expr)

    formula.enumerateVar.map( v =>
      (v, solver.getModel.evaluate(ctx.mkIntConst(v), false).asInstanceOf[IntNum].getInt)
    ).toMap
  }
}
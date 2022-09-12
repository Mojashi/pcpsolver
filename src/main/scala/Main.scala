import com.microsoft.z3.Context;
object Main extends App {
  val ctx = new Context()
  val solver = ctx.mkSolver()
  solver.add(
    ctx.mkEq(ctx.mkAdd(ctx.mkIntConst("x"), ctx.mkInt(123)), ctx.mkInt(1))
  )
  solver.check()
  print(solver.getModel.eval(ctx.mkIntConst("x"), false))
}
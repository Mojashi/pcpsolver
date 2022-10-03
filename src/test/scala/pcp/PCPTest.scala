package pcp

import org.scalatest.funsuite.AnyFunSuite
import com.microsoft.z3.{BoolExpr, Context, IntExpr, IntNum, Params, Status}

class PCPTest extends AnyFunSuite {
//  test("pcp parikh solver test") {
//    val pcp = PCP(List(
//     Tile("111", "010"),
//      Tile("111", "1"),
//      Tile("10", "100"),
//      Tile("0", "111")
//    ))
//
//    print(pcp)
//  }

  test("z3pcp") {
    val ctx = Context()

    val s1 = ctx.mkString("aaa") 
    val v = ctx.mkConst("out", ctx.mkStringSort())

    val s2 = ctx.mkEq(v, ctx.mkReplace(s1, ctx.mkToRe(ctx.mkString("a")), ctx.mkString("b")))

    val s = ctx.mkSolver()
    s.add(s2)

    s.check()
    val m = s.getModel
    println(m.evaluate(v, true).toString)
  }
}

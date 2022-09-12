package presburger
import org.scalatest.funsuite.AnyFunSuite

class PresburgerFormulaSolverTest extends AnyFunSuite {
  test("PresburgerFormulaSolver.solve") {
    assert(
      PresburgerFormulaSolver().solve(
        Equal(Constant(12), Variable("a"))
      ).get("a") == 12
    )
  }
}

package presburger
import org.scalatest.funsuite.AnyFunSuite

class PresburgerFormulaSolverTest extends AnyFunSuite {
  test("PresburgerFormulaSolver.solve") {
    assert(
      PresburgerFormulaSolver().solveWithCVC5(
        Equal(Constant(12), Variable("a"))
      ).get("a") == 12
    )
  }
}

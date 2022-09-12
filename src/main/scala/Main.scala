import com.microsoft.z3.{Context, IntNum, IntSort}
import transducer.{StringTransducer, Transition, parikhAutomaton, solve}
import pcp.{PCP, Tile}
import presburger.{PresburgerFormulaSolver, And, Equal, Variable, Constant}
object Main extends App {
  val pcp = PCP(List(
    Tile("abc", "cbbbbd"),
    Tile("accd", "cbbbbd"),
  ))

  val t = pcp.transducers._1
  print(t.solve(
    And(
      Equal(Variable("a"), Constant(12)),
      Equal(Variable("d"), Constant(2)),
    )
  ))
}

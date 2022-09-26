import com.microsoft.z3.{Context, IntNum, IntSort}
import transducer.*
import pcp.*
import presburger.{PresburgerFormulaSolver, And, Equal, Variable, Constant}
object Main extends App {
  val pcp = PCP(List(
    Tile("100", "1"),
    Tile("0", "100"),
    Tile("1", "00"),
  ))
//  println(pcp.solve)
}

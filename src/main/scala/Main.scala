import com.microsoft.z3.{Context, IntNum, IntSort}
import transducer.*
import pcp.*
import presburger.{PresburgerFormulaSolver, And, Equal, Variable, Constant}
object Main extends App {
//  val pcp = PCP(List(
//    Tile("111", "010"),
//    Tile("111", "1"),
//    Tile("10", "100"),
//    Tile("0", "111")
//  ))
  val pcp = PCP(List(
    Tile("100", "1"),
    Tile("0", "100"),
    Tile("1", "00"),
  ))
  println(pcp.solve)
}

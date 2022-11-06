import com.microsoft.z3.{Context, IntNum, IntSort}
import transducer.*
import pcp.*
import presburger.{PresburgerFormulaSolver, And, Equal, Variable, Constant}
import solver.{ParallelNFA, ParalellTransducerNFA}
object Main extends App {
  val pcp = Instances.pcp34Unsolved
  val (t1, t2) = pcp.transducers
  t1.saveSVG[Int]("t1")
  t2.saveSVG[Int]("t2")

  SubStrCountTransducer("101", pcp.alphabets).stateAny().saveSVG[Int]("101")
  SubStrCountTransducer("1001", pcp.alphabets).stateAny().saveSVG[Int]("1001")
  SubStrCountTransducer("101", pcp.alphabets).stateAny().product(
    SubStrCountTransducer("1001", pcp.alphabets).stateAny()
  ).saveSVG[Int]("comb")
}
import com.microsoft.z3.{Context, IntNum, IntSort}
import transducer.{StringTransducer, Transition}
object Main extends App {
  val trans = StringTransducer (
    "0", Set("fin"), Set(
      Transition("0", "1", )
    )
  )
}

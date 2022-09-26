package flatAutomaton
import automaton.{NFA, Transition}
import graph.UniqueEdgeId
import presburger.*
import graph.EdgeUseCountVar

def flatState(cycle: Int, repeat: Int): String = s"c${cycle}_t${repeat}"

class FlatAutomaton[Alphabet]
(
  val name: String,
  val cycleSize: Int,
  val repeatTime: Int,
  override val alphabets: Set[Alphabet],
) extends NFA[String, Alphabet]({
  NFA(
    start = flatState(0,0),
    fin = Set(flatState(repeatTime-1, 0)),
    transitions = (0 until repeatTime).flatMap(c =>
      (0 until cycleSize).flatMap(t =>
        (alphabets.map(c=>Some(c)).toSeq :+ None).map( ch =>
          Transition(
            from = flatState(c, t),
            to = flatState(c, (t + 1) % cycleSize),
            in = ch,
            id = s"flat_${name}($c,$t,$ch)"
          )
        )
      )
    ) ++
      (0 until repeatTime - 1).map(c =>
        Transition(
          from = flatState(c, 0),
          to = flatState(c + 1, 0),
          in = None,
          id = s"flat_${name}_next($c)"
        )
      )
  )
}) {


  val purityConstraint = {
    val as = alphabets.map(c=>Some(c)).toSeq :+ None
    AndList(
      (0 until repeatTime).flatMap(c =>
        (0 until cycleSize).flatMap(t =>
          as.zipWithIndex.flatMap((ch1, idx1) =>
            as.drop(idx1 + 1).map(ch2 =>
              Or(
                Equal(EdgeUseCountVar(s"flat_${name}($c,$t,$ch1)"), Constant(0)),
                Equal(EdgeUseCountVar(s"flat_${name}($c,$t,$ch2)"), Constant(0))
              )
            )
          )
        )
      )
    )
  }

}

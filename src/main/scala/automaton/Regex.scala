//package automaton
//
//trait Regex {
//  def nfa: NFA[String, Char]
//}
//case class Or(r1: Regex, r2: Regex) extends Regex {
//  override def nfa: NFA = NFA()
//}
//case class Constant(s: Char) extends Regex{
//  override def nfa: NFA = NFA(start = "0", fin = Set("0"), transitions=Seq(Transition("0", "0", s)))
//}
//case class Star(r: Regex) extends Regex
//

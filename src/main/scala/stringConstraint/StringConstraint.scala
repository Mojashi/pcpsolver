package stringConstraint
import automaton.NFA
import transducer.Transducer

type VarName = String
type StringAssigns = Map[VarName, String]

trait StringConstraint[State]

case class NFAAccept[State](v:Variable, nfa: NFA[State, Char]) extends StringConstraint[State]
case class Transduce[State](in: Atom, transducer: Transducer[State, Char, List[Char]]) extends StringConstraint[State]
case class Equal[State](l: Atom, r: Atom) extends StringConstraint[State]
case class Concat[State](ins: List[Atom], o: Variable)

trait Atom
case class Variable(varName: VarName) extends Atom
case class Constant(s: String) extends Atom

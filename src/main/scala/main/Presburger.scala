package main

import com.microsoft.z3.{Context as Z3Context, BoolExpr}

type VarName = String
type VarValueMap = Map[VarName, Int]

trait ExistentialPresburgerFormula {
  def eval(m: VarValueMap): Boolean
}
type EPF = ExistentialPresburgerFormula

case class Equal(left: PE, right: PE) extends ExistentialPresburgerFormula {
  override def eval(m: VarValueMap): Boolean = left.eval(m) == right.eval(m)
}
case class GreaterThan(left: PE, right: PE) extends ExistentialPresburgerFormula {
  override def eval(m: VarValueMap): Boolean = left.eval(m) > right.eval(m)
}
case class And(left: EPF, right: EPF) extends ExistentialPresburgerFormula {
  override def eval(m: VarValueMap): Boolean = left.eval(m) && right.eval(m)
}
case class Or(left: EPF, right: EPF) extends ExistentialPresburgerFormula {
  override def eval(m: VarValueMap): Boolean = left.eval(m) || right.eval(m)
}

trait PresburgerExpression {
  def eval(m: VarValueMap): Int
}
case class Constant(v: Int) extends PE {
  override def eval(m: VarValueMap): Int = v
}
case class Variable(name: VarName) extends PE {
  override def eval(m: VarValueMap): Int = m(name)
}
case class Add(left: PE, right: PE) extends PE {
  override def eval(m: VarValueMap): Int = left.eval(m) + right.eval(m)
}

type PE = PresburgerExpression
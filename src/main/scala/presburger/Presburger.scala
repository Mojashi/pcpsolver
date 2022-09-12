package presburger

import com.microsoft.z3.{ArithExpr, BoolExpr, Context, IntExpr, IntSort}

type VarName = String
type VarValueMap = Map[VarName, Int]

trait ExistentialPresburgerFormula {
  def eval(implicit m: VarValueMap): Boolean
  def z3Expr(implicit ctx: Context): BoolExpr
  def enumerateVar: Set[VarName]
}
private type EPF = ExistentialPresburgerFormula

case class Equal(left: PE, right: PE) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap): Boolean = left.eval(m) == right.eval(m)

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkEq(left.z3Expr, right.z3Expr)

  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar
}

case class GreaterThan(left: PE, right: PE) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap): Boolean = left.eval(m) > right.eval(m)

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkGt(left.z3Expr, right.z3Expr)
  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar
}

case class GreaterThanOrEqual(left: PE, right: PE) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap): Boolean = left.eval(m) >= right.eval(m)
  
  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkGe(left.z3Expr, right.z3Expr)
  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar
}
case class And(left: EPF, right: EPF) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap): Boolean = left.eval(m) && right.eval(m)

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkAnd(left.z3Expr, right.z3Expr)
  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar
}
case class Or(left: EPF, right: EPF) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap): Boolean = left.eval(m) || right.eval(m)

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkOr(left.z3Expr, right.z3Expr)
  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar
}

case object True extends ExistentialPresburgerFormula {
  override def enumerateVar: Set[VarName] = Set()

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkBool(true)

  override def eval(implicit m: VarValueMap): Boolean = true
}

case object False extends ExistentialPresburgerFormula {
  override def enumerateVar: Set[VarName] = Set()

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkBool(false)

  override def eval(implicit m: VarValueMap): Boolean = false
}

trait PresburgerExpression {
  def eval(implicit m: VarValueMap): Int
  def z3Expr(implicit ctx: Context): ArithExpr[IntSort]
  def enumerateVar: Set[VarName]
}
case class Constant(v: Int) extends PE {
  override def eval(implicit m: VarValueMap): Int = v

  override def z3Expr(implicit ctx: Context) = ctx.mkInt(v)

  override def enumerateVar: Set[VarName] = Set()
}
case class Variable(name: VarName) extends PE {
  override def eval(implicit m: VarValueMap): Int = m(name)

  override def z3Expr(implicit ctx: Context) = ctx.mkIntConst(name)

  override def enumerateVar: Set[VarName] = Set(name)
}
case class Add(left: PE, right: PE) extends PE {
  override def eval(implicit m: VarValueMap): Int = left.eval(m) + right.eval(m)

  override def z3Expr(implicit ctx: Context) = ctx.mkAdd(left.z3Expr, right.z3Expr)

  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar
}

// for convenience
// "x-y" is shorthand of "x+z and z+y=0"
case class Sub(left: PE, right: PE) extends PE {
  override def eval(implicit m: VarValueMap): Int = left.eval(m) - right.eval(m)

  override def z3Expr(implicit ctx: Context) = ctx.mkSub(left.z3Expr, right.z3Expr)

  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar
}

// ????
case class Mul(left: PE, right: PE) extends PE {
  override def eval(implicit m: VarValueMap): Int = left.eval(m) * right.eval(m)

  override def z3Expr(implicit ctx: Context) = ctx.mkMul(left.z3Expr, right.z3Expr)

  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar
}

def AndList(fs: List[EPF]): EPF = fs.reduceOption((l,r) => And(l,r)).getOrElse(True)
def OrList(fs: List[EPF]): EPF = fs.reduceOption((l,r) => Or(l,r)).getOrElse(False)

private type PE = PresburgerExpression

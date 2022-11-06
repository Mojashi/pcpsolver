package presburger

import com.microsoft.z3.{ArithExpr, BoolExpr, Context, IntExpr, IntSort}
import io.github.cvc5.{Kind, Solver as CVCSolver, Term as CVCTerm}
import scala.collection.mutable.Map as MutableMap

type VarName = String
type VarValueMap[T] = Map[VarName, T]

case class CVCContext
(
  solver: CVCSolver,
  variableRegistry: MutableMap[String, CVCTerm]
)

trait ExistentialPresburgerFormula {
  def eval(implicit m: VarValueMap[Int]): Boolean
  def z3Expr(implicit ctx: Context): BoolExpr

  def cvcExpr(implicit ctx: CVCContext): CVCTerm
  def enumerateVar: Set[VarName]
  def map(f:AtomicPresburgerExpression=>PresburgerExpression): ExistentialPresburgerFormula
}
private type EPF = ExistentialPresburgerFormula
case class Not(epf: EPF) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap[Int]): Boolean = !epf.eval(m)

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkNot(epf.z3Expr)

  override def enumerateVar: Set[VarName] = epf.enumerateVar

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = epf.map(f)

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = epf.cvcExpr.notTerm()
}
case class Equal(left: PE, right: PE) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap[Int]): Boolean = left.eval(m) == right.eval(m)

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkEq(left.z3Expr, right.z3Expr)

  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = Equal(left.map(f), right.map(f))

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.solver.mkTerm(Kind.EQUAL, left.cvcExpr, right.cvcExpr)
}

case class GreaterThan(left: PE, right: PE) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap[Int]): Boolean = left.eval(m) > right.eval(m)

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkGt(left.z3Expr, right.z3Expr)
  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = GreaterThan(left.map(f), right.map(f))

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.solver.mkTerm(Kind.GT, left.cvcExpr, right.cvcExpr)
}

case class GreaterThanOrEqual(left: PE, right: PE) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap[Int]): Boolean = left.eval(m) >= right.eval(m)
  
  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkGe(left.z3Expr, right.z3Expr)
  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = GreaterThanOrEqual(left.map(f), right.map(f))

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.solver.mkTerm(Kind.GEQ, left.cvcExpr, right.cvcExpr)
}
case class And(left: EPF, right: EPF) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap[Int]): Boolean = left.eval(m) && right.eval(m)

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkAnd(left.z3Expr, right.z3Expr)
  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = And(left.map(f), right.map(f))

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = left.cvcExpr.andTerm(right.cvcExpr)
}
case class Or(left: EPF, right: EPF) extends ExistentialPresburgerFormula {
  override def eval(implicit m: VarValueMap[Int]): Boolean = left.eval(m) || right.eval(m)

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkOr(left.z3Expr, right.z3Expr)
  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = Or(left.map(f), right.map(f))

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = left.cvcExpr.orTerm(right.cvcExpr)
}

case object True extends ExistentialPresburgerFormula {
  override def enumerateVar: Set[VarName] = Set()

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkBool(true)

  override def eval(implicit m: VarValueMap[Int]): Boolean = true

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = this

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.solver.mkTrue()
}

case object False extends ExistentialPresburgerFormula {
  override def enumerateVar: Set[VarName] = Set()

  override def z3Expr(implicit ctx: Context): BoolExpr = ctx.mkBool(false)

  override def eval(implicit m: VarValueMap[Int]): Boolean = false

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = this

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.solver.mkFalse()
}

trait PresburgerExpression {
  def eval(implicit m: VarValueMap[Int]): Int
  def z3Expr(implicit ctx: Context): ArithExpr[IntSort]
  def cvcExpr(implicit ctx: CVCContext): CVCTerm
  def enumerateVar: Set[VarName]
  def map(f: AtomicPresburgerExpression => PresburgerExpression): PresburgerExpression
}
trait AtomicPresburgerExpression extends PresburgerExpression
case class Constant(v: Int) extends PE, AtomicPresburgerExpression {
  override def eval(implicit m: VarValueMap[Int]): Int = v

  override def z3Expr(implicit ctx: Context) = ctx.mkInt(v)

  override def enumerateVar: Set[VarName] = Set()

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.solver.mkInteger(v)

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = f(this)
}
case class Variable(name: VarName) extends PE, AtomicPresburgerExpression {
  override def eval(implicit m: VarValueMap[Int]): Int = m(name)

  override def z3Expr(implicit ctx: Context) = ctx.mkIntConst(name)

  override def enumerateVar: Set[VarName] = Set(name)

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = f(this)

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.variableRegistry.getOrElseUpdate(name, ctx.solver.mkConst(ctx.solver.getIntegerSort, name))
}
case class Add(left: PE, right: PE) extends PE {
  override def eval(implicit m: VarValueMap[Int]): Int = left.eval(m) + right.eval(m)

  override def z3Expr(implicit ctx: Context) = ctx.mkAdd(left.z3Expr, right.z3Expr)

  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = Add(left.map(f), right.map(f))

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.solver.mkTerm(Kind.ADD, left.cvcExpr, right.cvcExpr)
}

// for convenience
case class Sub(left: PE, right: PE) extends PE {
  override def eval(implicit m: VarValueMap[Int]): Int = left.eval(m) - right.eval(m)

  override def z3Expr(implicit ctx: Context) = ctx.mkSub(left.z3Expr, right.z3Expr)

  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = Sub(left.map(f), right.map(f))

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.solver.mkTerm(Kind.SUB, left.cvcExpr, right.cvcExpr)
}

// ????
case class Mul(left: Constant, right: PE) extends PE {
  override def eval(implicit m: VarValueMap[Int]): Int = left.eval(m) * right.eval(m)

  override def z3Expr(implicit ctx: Context) = ctx.mkMul(left.z3Expr, right.z3Expr)

  override def enumerateVar: Set[VarName] = left.enumerateVar ++ right.enumerateVar

  override def map(f: AtomicPresburgerExpression => PresburgerExpression) = Mul(left, right.map(f))

  override def cvcExpr(implicit ctx: CVCContext): CVCTerm = ctx.solver.mkTerm(Kind.MULT, left.cvcExpr, right.cvcExpr)
}

def AndList(fs: Seq[EPF]): EPF = fs.reduceOption((l,r) => And(l,r)).getOrElse(True)
def OrList(fs: Seq[EPF]): EPF = fs.reduceOption((l,r) => Or(l,r)).getOrElse(False)

private type PE = PresburgerExpression

extension (epf: ExistentialPresburgerFormula) {
  def prettyPrint(): String = {
    epf match {
      case And(l,r) => {
        val lp = l.prettyPrint()
        val rp = r.prettyPrint()
        lp.split("\n").map(line => "\t" + line).mkString("\n")
        rp.split("\n").map(line => "\t" + line).mkString("\n")
        lp +"\n"+ rp
      }
      case other => other.toString
    }
  }
}
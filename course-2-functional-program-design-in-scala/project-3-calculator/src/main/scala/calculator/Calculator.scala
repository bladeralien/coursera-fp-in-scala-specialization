package calculator

import scala.collection.mutable.ListBuffer

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    // TODO avoid using var
    var values: Map[String, Signal[Double]] = Map()
    for ((name: String, expr: Signal[Expr]) <- namedExpressions) {
      values += ((name, Signal(eval(expr(), namedExpressions))))
    }
    values
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    // TODO avoid using mutable collections
    def helper(expr: Expr, references: Map[String, Signal[Expr]], refStack: ListBuffer[String]): Double = {
      expr match {
        case literal: Literal => literal.v
        case ref: Ref => {
          if (refStack.contains(ref.name)) {
            Double.NaN
          } else {
            refStack.+=(ref.name)
            val value = helper(getReferenceExpr(ref.name, references), references, refStack:+ref.name)
            refStack.-=(ref.name)
            value
          }
        }
        case plus: Plus => helper(plus.a, references, refStack) + helper(plus.b, references, refStack)
        case minus: Minus => helper(minus.a, references, refStack) - helper(minus.b, references, refStack)
        case times: Times => helper(times.a, references, refStack) * helper(times.b, references, refStack)
        case divide: Divide => helper(divide.a, references, refStack) / helper(divide.b, references, refStack)
      }
    }
    helper(expr, references, ListBuffer())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}

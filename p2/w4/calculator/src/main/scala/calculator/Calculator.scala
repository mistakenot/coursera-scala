package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (k, v) => {
        (k, Signal(eval(v(), namedExpressions)))
      }
    }
  }

  val nan = Signal(Literal(Double.NaN))

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def inner(e: Expr, seen: Set[String]): Double = e match {
      case Literal(v) => v
      case Ref(name) =>
        if (seen contains name) Double.NaN
        else inner(references.getOrElse(name, nan)(), seen + name)
      case Plus(a, b) => inner(a, seen) + inner(b, seen)
      case Minus(a, b) => inner(a, seen) - inner(b, seen)
      case Times(a, b) => inner(a, seen) * inner(b, seen)
      case Divide(a, b) => inner(a, seen) / inner(b, seen)
    }

    inner(expr, Set())
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

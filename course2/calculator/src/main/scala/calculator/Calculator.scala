package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def mapExp (exp: Signal[Expr], namedExpressions: Map[String, Signal[Expr]], visitedSoFar:Set[String]) : Signal[Double] = Signal {
    exp() match {
      case Literal(v) => v
      case Ref(name)=>{
        if(visitedSoFar contains(name))
          Double.NaN
        else
          mapExp(Signal(getReferenceExpr(name, namedExpressions)),namedExpressions,visitedSoFar+name)()
      }
      case Plus(a: Expr, b: Expr) => mapExp(Signal(a), namedExpressions,visitedSoFar)() + mapExp(Signal(b), namedExpressions,visitedSoFar)()
      case Minus(a: Expr, b: Expr) => mapExp(Signal(a), namedExpressions, visitedSoFar)() -  mapExp(Signal(b), namedExpressions, visitedSoFar)()
      case Times(a: Expr, b: Expr) => mapExp(Signal(a), namedExpressions, visitedSoFar)() *  mapExp(Signal(b), namedExpressions, visitedSoFar)()
      case Divide(a: Expr, b: Expr) => mapExp(Signal(a), namedExpressions, visitedSoFar)() /  mapExp(Signal(b), namedExpressions, visitedSoFar)()
    }
  }

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    (namedExpressions toList).map({case (k,v)=> (k,mapExp(v,namedExpressions,Set()))}) toMap
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    mapExp(Signal(expr), references, Set())()
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

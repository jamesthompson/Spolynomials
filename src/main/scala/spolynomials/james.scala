import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

/*  Univariate Polynomial Class
    
    Companion object builder pattern works thus : Polynomial(5 -> 4.2, 4 -> 3.1, 2 -> 2.0), 
    where key = Order (an Int), value = Coefficient
    
    Mathematical operations work as normally as you'd expect due to Ring instance for all Polynomial[R] types
    (created by Tom Switzer)
*/

type Order = Int // Only natural exponents are applicable

final class Polynomial[R](val terms: Map[Order, R]) {

  // Evaluate for a given variable (symbol) in R.
  def apply(x: R)(implicit R: Ring[R]): R =
    terms.map({ case (i, c) => c * (x ** i) }).foldLeft(R.zero)(_ + _)

  // Normalize the polynomial, the largest order term coefficient will then be === R.one
  def monic(implicit R: Ring[R],
                     G: MultiplicativeGroup[R], 
                     O: Ordering[R]) : Polynomial[R] = {
    val maxCoeffTerm = terms.map(_._2).max
    new Polynomial(terms.toMap map { case (k,v) => k -> v / maxCoeffTerm })
  }



  // Formats polynomials in descending order. n.b. 1x^0 = 1 and 1x^1 = 1x here
  override def toString = { terms.toList.sortBy(_._1).reverse map { 
    case (i, c) => i match {
      case 0 => s"$c"
      case 1 => s"${c}x"
      case _ => s"${c}x^$i"
      }
    } mkString " + "
  }

}

object Polynomial {

  implicit def ring[R: Ring] = new PolynomialRing[R] {
    val R = Ring[R]
  }

  def apply[R: Ring](cs: (Order, R)*): Polynomial[R] =
    new Polynomial(cs.toMap)

}

trait PolynomialRing[R] extends Ring[Polynomial[R]] {
  implicit def R: Ring[R]

  def zero = new Polynomial(Map.empty)

  def one = new Polynomial(Map(0 -> R.one))

  def plus(x: Polynomial[R], y: Polynomial[R]): Polynomial[R] =
    new Polynomial(x.terms + y.terms)

  def negate(x: Polynomial[R]): Polynomial[R] =
    new Polynomial(x.terms mapValues R.negate)

  def times(x: Polynomial[R], y: Polynomial[R]): Polynomial[R] =
    x.terms.foldLeft(zero) { case (p, (i, c0)) =>
      plus(p, new Polynomial(y.terms map { case (j, c1) => (i + j) -> c0 * c1 }))
  }
}

// Specific Cases

object SpecialPolynomials {

  // Returns a list of legendre polynomials (Rational type) up to i
  def legendres(i: Int)(implicit R: Numeric[Rational]) : List[Polynomial[Rational]] = {
    val one = Polynomial(0 -> R.one)
    val x = Polynomial(1 -> R.one)
    lazy val leg : Stream[Polynomial[Rational]] = {
      def loop(pnm1: Polynomial[Rational], pn: Polynomial[Rational], n: Int = 1) : Stream[Polynomial[Rational]] = {
        pn #:: loop(pn, Polynomial(0 -> R.fromInt(n + 1).reciprocal) * (
                    (Polynomial(1 -> R.fromInt(2 * n + 1)) * pn) + 
                    (Polynomial(0 -> R.fromInt(n).unary_-) * pnm1)), n + 1)
      }
      one #:: loop(one, x)
    }
    leg.take(i).toList
  }

}

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

type Order = Int // Only integer exponents are applicable

final class Polynomial[R](val terms: Map[Order, R]) {

  // Evaluate for a given variable (symbol) in R.
  def apply(x: R)(implicit R: Ring[R]) : R =
    terms.map({ case (i, c) => c * (x ** i) }).foldLeft(R.zero)(_ + _)

  // Is this polynomial zero?
  def isZero : Boolean = terms.isEmpty

  // Normalize the polynomial, the largest order term coefficient will then be === R.one
  def monic(implicit R: Ring[R],
                     G: MultiplicativeGroup[R], 
                     O: Ordering[R]) : Polynomial[R] = {
    val maxCoeffTerm = terms.map(_._2).max
    new Polynomial(terms.toMap map { case (k,v) => k -> v / maxCoeffTerm })
  }

  // Computes the derivative of the polynomial
  def derivative(implicit R: Ring[R]) : Polynomial[R] =
    new Polynomial(terms.filterNot(_._1 == 0).map { case (k, v) => k - 1 -> v * R.fromInt(k) } )

  // Computer the indefinite integral of the polynomial + (n.b. plus a constant)
  def integral(implicit R: Ring[R],
                        G: MultiplicativeGroup[R]) : Polynomial[R] =
    new Polynomial(terms.map { case (k, v) => k + 1 -> v / R.fromInt(k + 1) } )

  // Formats polynomials in descending order. n.b. 1x^0 = "1" and -1x^1 = "-1x" here
  override def toString = { terms.toList.sortBy(_._1).reverse map { 
    case (i, c) => i match {
      case 0 => s"$c"
      case 1 => s"${c}x"
      case _ => s"${c}x^$i"
      }
    } mkString " + "
  }

}

// Companion object
object Polynomial {

  implicit def ring[R: Ring] = new PolynomialRing[R] {
    val R = Ring[R]
  }

  def apply[R: Ring](cs: (Order, R)*): Polynomial[R] =
    new Polynomial(cs.toMap)

}

// Polynomial Ring Instance - shouldn't it be a EuclideanRing???
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

  // Euclidean Ring functions
  // def quot(a: Polynomial[R], b: Polynomial[R]): Polynomial[R] = ???
  
  def mod(a: Polynomial[R], b: Polynomial[R]): Polynomial[R] = 
    require(!b.isZero, "Can't divide a polynomial by zero")
    

  def gcd(a: Polynomial[R], b: Polynomial[R]): Polynomial[R] = 
    require(!a.isZero || !b.isZero, "At lease one of the polynomials must be non-zero.")
    if(!a.isZero) a.monic else gcd(b, a mod b)

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

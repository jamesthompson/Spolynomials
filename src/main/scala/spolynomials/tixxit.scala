import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Working Backup... 24 / 07 / 13

type Order = Int

final class Polynomial[R](val terms: Map[Order, R]) {
  override def toString = {
    terms.toList.sortBy(_._1).reverse map { case (i, c) =>
      if(i > 1) s"${c}x^$i" else if(i === 0) s"${c}" else s"${c}x"
    } mkString " + "
  }

  // Evaluate for a given variable (symbol) in R.
  def apply(x: R)(implicit R: Ring[R]): R =
    terms.map({ case (i, c) => c * (x ** i) }).foldLeft(R.zero)(_ + _)
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
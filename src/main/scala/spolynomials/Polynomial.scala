package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

sealed trait Endianness
case object BE extends Endianness
case object LE extends Endianness

final class Polynomial[R](val end: Endianness, 
													val coeffs: Vector[R])
												 (implicit R: Ring[R]) {

	implicit val swapEnd : Endianness = this.end match {
		case BE => LE
		case LE => BE
	}

	def apply(x: R) : R = 
		makeTerms.map({ case (c, i) => c * (x ** i) }).foldLeft(R.zero)(_ + _)

	def makeTerms : Vector[(R, Int)] = end match {
		case BE => coeffs.zip((0 until coeffs.length).reverse)
		case LE => coeffs.zip((0 until coeffs.length))
	}

	def isZero : Boolean = coeffs.isEmpty

	def monic(implicit G: MultiplicativeGroup[R]) : Polynomial[R] = end match {
		case BE => new Polynomial(BE, coeffs.map(_ / coeffs.head))
		case LE => new Polynomial(LE, coeffs.map(_ / coeffs.last))
	}

	def swapEndianness : Polynomial[R] = end match {
		case BE => new Polynomial(LE, coeffs.reverse)
		case LE => new Polynomial(BE, coeffs.reverse)
	}

	def derivative : Polynomial[R] = 
		new Polynomial(this.end, makeTerms.filterNot(_._2 == 0).map({case (c, i) => c * i}))

	def integral(implicit G: MultiplicativeGroup[R]) : Polynomial[R] = {
		val intTerms = makeTerms.map({case (c, i) => c / R.fromInt(i + 1)})
		end match {
			case BE => new Polynomial(this.end, intTerms :+ R.fromInt(0))
			case LE => new Polynomial(this.end, R.fromInt(0) +: intTerms)
		}
	}

	// A correctly formatted polynomial
	override def toString = makeTerms map {
		case (c, i) => (c, i) match {
			case (0, i) => ""
			case (1, 1) => "x"
			case (1, 0) => "1"
			case (c, 1) => s"${c}x"
			case (1, i) => s"x^$i"
			case (c, 0) => s"${c}"
			case (c, i) => s"${c}x^$i"
		}
	} mkString(" + ")

}

object Polynomial {

	// implicit def eucRing[R: EuclideanRing] = new PolynomialRing[R] {
	// 	val R = EuclideanRing[R]
	// }

	def apply[R: Ring](end: Endianness, coeffs: R*) : Polynomial[R] =
		new Polynomial(end, coeffs.toVector)

}

trait PolynomialRing[R] extends EuclideanRing[Polynomial[R]] {

  implicit def R : EuclideanRing[R]

  def zero = new Polynomial(BE, Vector(R.fromInt(0)))

  def one = new Polynomial(BE, Vector(R.fromInt(1)))

  def plus(x: Polynomial[R], y: Polynomial[R]) : Polynomial[R] = ???

  def negate(x: Polynomial[R]): Polynomial[R] = ???

  def times(x: Polynomial[R], y: Polynomial[R]) : Polynomial[R] = ???
  //   x.terms.foldLeft(zero) { case (p, (i, c0)) =>
  //     plus(p, new Polynomial(y.terms map { case (j, c1) => (i + j) -> c0 * c1 }))
  // }

  // Euclidean Ring functions
  // def quot(a: Polynomial[R], b: Polynomial[R]): Polynomial[R] = ???
  
  def mod(a: Polynomial[R], b: Polynomial[R]) : Polynomial[R] = {
    require(!b.isZero, "Can't divide a polynomial by zero")
  }


  def gcd(a: Polynomial[R], b: Polynomial[R]) : Polynomial[R] = 
    require(!a.isZero || !b.isZero, "At lease one of the polynomials must be non-zero.")
    if(!a.isZero) a.monic else gcd(b, mod(a,b))

}
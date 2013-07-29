package spolynomials

import scala.math.{Ordering => ScalaOrdering} 
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

case class Term[R: Ring](val coeff: R, val index: Int)

trait TermOrder[R] extends Order[Term[R]] {
	override def eqv(x:Term[R], y:Term[R]): Boolean = x.index == y.index
	override def neqv(x:Term[R], y:Term[R]): Boolean = x.index != y.index
	def compare(x: Term[R], y:Term[R]): Int =
		if(x.index < y.index) -1 else if(eqv(x, y)) 0 else 1
}


final class Poly[R](val terms: List[Term[R]])
									 (implicit R: Ring[R]) {

implicit def TermOrder[R: Ring] = new TermOrder[R] {}

implicit def BigEndianOrdering[R: Ring] = new ScalaOrdering[Term[R]] {
  def compare(x:Term[R], y:Term[R]) =
  	if(x.index < y.index) 1 else if(x == y) 0 else -1
}

	def apply(x: R): R = ???
}






final class Polynomial[R](val end: Endianness, 
													val coeffs: Vector[R])
												 (implicit R: Ring[R]) {

	def apply(x: R) : R = 
		terms.map({ case (c, i) => c * (x ** i) }).foldLeft(R.zero)(_ + _)

	lazy val terms : Map[R, Int] = end match {
		case BE => coeffs.zip((0 until coeffs.length).reverse).toMap
		case LE => coeffs.zip((0 until coeffs.length)).toMap
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

	def makeLE : Polynomial[R] = if(end == BE) swapEndianness else this

	def makeBE : Polynomial[R] = if(end == LE) swapEndianness else this

	def derivative : Polynomial[R] = 
		new Polynomial(this.end, terms.filterNot(_._2 == 0).map({case (c, i) => c * i}).toVector)

	def integral(implicit G: MultiplicativeGroup[R]) : Polynomial[R] = {
		val intTerms = terms.map({case (c, i) => c / R.fromInt(i + 1)}).toVector
		end match {
			case BE => new Polynomial(this.end, intTerms :+ R.fromInt(0))
			case LE => new Polynomial(this.end, R.fromInt(0) +: intTerms)
		}
	}

	override def toString = 
		checkString( terms.sortBy(_._2) map {
		case (c, i) => (c, i) match {
			case (0, i) => ""
			case (1, 1) => "x"
			case (1, 0) => "1"
			case (c, 1) => s"${c}x"
			case (1, i) => s"x^$i"
			case (c, 0) => s"${c}"
			case (c, i) => s"${c}x^$i"
		}
	} mkString(" + "))

	def checkString(s: String) : String =
		if(s.reverse.take(3) == " + ") checkString(s.dropRight(3)) else s

}

object Polynomial {

	// implicit def eucRing[R: EuclideanRing] = new PolynomialRing[R] {
	// 	val R = EuclideanRing[R]
	// }

	def apply[R: Ring](end: Endianness, terms: (R, Int)*) : Polynomial[R] = {
		val maxIndex = terms.map(_._2).max
		val padders = for { i <- 0 to maxIndex;
					if(!terms.map(_._2).contains(i))		
		} yield (0, i) // might have a prob with 0 not being an 'R'
		val newTerms : Map[R, Int] = padders ++ terms
		new Polynomial(end, newTerms.sortBy(_._2).map(_._1).toVector)
	}

}

trait PolynomialRing[R] extends EuclideanRing[Polynomial[R]] {

  implicit def R : EuclideanRing[R]

  def zero = new Polynomial(BE, Vector(R.zero))

  def one = new Polynomial(BE, Vector(R.one))

  def plus(x: Polynomial[R], y: Polynomial[R]) : Polynomial[R] = {
  	val addedTerms = (x.makeTerms.toMap + y.makeTerms.toMap)
  	val newCoeffs = for(i <- 0 until addedTerms.map(_._2).max) yield {
  		if(addedTerms.contains(i)) addedTerms.get(i).get else 0
  	}
  	new Polynomial(BE, newCoeffs)
	}

  def negate(x: Polynomial[R]): Polynomial[R] =
  	new Polynomial(e.end, x.terms.map(R.negate))

  def times(x: Polynomial[R], y: Polynomial[R]) : Polynomial[R] = {
   //  x.makeBE.makeTerms.foldLeft(zero) { case (p, (c0, i)) =>
   //    plus(p, new Polynomial(y.makeBE.makeTerms map { case (c1, j) => (i + j) -> c0 * c1 }))
  	// }
  	zero
  }

  // Euclidean Ring functions
  def quot(a: Polynomial[R], b: Polynomial[R]): Polynomial[R] = zero
  
  def mod(a: Polynomial[R], b: Polynomial[R]) : Polynomial[R] = zero

  def gcd(a: Polynomial[R], b: Polynomial[R]) : Polynomial[R] = zero

}

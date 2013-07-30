package SPolynomials

import scala.math.{Ordering => ScalaOrdering} 
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomial term
case class Term[F: Field](val coeff: F, val index: Int) {

	def eval(x: F): F = coeff * (x ** index)

	def isIndexZero: Boolean = index == 0

	def divideBy(x: F): Term[F] = Term(coeff / x, index)

	def der: Term[F] = Term(coeff * index, index - 1)

	def int: Term[F] = Term(coeff / (index + 1), index + 1)

	override def toString = (coeff, index) match {
		case (0, i) => ""
		case (1, 1) => "x"
		case (1, 0) => "1"
		case (c, 1) => s"${c}x"
		case (1, i) => s"x^$i"
		case (c, 0) => s"${c}"
		case (c, i) => s"${c}x^$i"
	}
}

// Univariate polynomial terms form a ring
trait TermRing[F] extends Ring[Term[F]] {

	implicit def F: Field[F]

	def negate(t: Term[F]): Term[F] =
		Term(t.coeff.unary_-, t.index)

	def zero: Term[F] = Term(F.zero, 0)

	def one: Term[F] = Term(F.one, 0)

	def plus(x: Term[F], y: Term[F]): Term[F] = 
		Term(x.coeff + y.coeff, y.index)

	def times(x: Term[F], y: Term[F]): Term[F] = 
		Term(x.coeff * y.coeff, x.index + y.index)

}

// Univariate polynomial class
final class Poly[F](val terms: List[Term[F]])
									 (implicit F: Field[F]) {

	implicit object BigEndianPolyOrdering extends ScalaOrdering[Term[F]] {
	  def compare(x:Term[F], y:Term[F]) : Int =
	  	if(x.index < y.index) 1 else if(x.index == y.index) 0 else -1
	}

	lazy val coeffs: List[F] = {
		val indices = terms.sorted.map(_.index)
		val fillerTerms = for {
			i <- 0 to maxOrder;
			if(!indices.contains(i))
		} yield Term(F.zero, i)
		(terms ++ fillerTerms.toList).sorted.map(_.coeff)
	}

	// n.b. we have big endian ordering hence .min
	lazy val maxTerm: Term[F] = isEmpty match {
		case true => Term(F.zero, 0)
		case false => terms.min 
	}

	lazy val maxOrder: Int = maxTerm.index

	lazy val maxOrderTermCoeff: F = maxTerm.coeff
	
	def apply(x: F): F = terms.map(_.eval(x)).foldLeft(F.zero)(_ + _)

	def isZero: Boolean = maxOrder == 0 && maxOrderTermCoeff == F.zero

	def isEmpty: Boolean = terms.isEmpty

	def monic: Poly[F] = isEmpty match {
		case true => this
		case false => new Poly(terms.map(_.divideBy(maxOrderTermCoeff)))
	}
	
	def derivative: Poly[F] = new Poly(terms.filterNot(_.isIndexZero).map(_.der))
	
	def integral: Poly[F] = new Poly(terms.map(_.int))
	
	override def toString = checkString(terms.sorted.mkString(" + "))
	
	private def checkString(s: String) : String = 
		if(s.reverse.take(3) == " + ") checkString(s.dropRight(3)) else s

}

// Companion object for Poly
object Poly {

	implicit def ring[F: Field] = new PolynomialRing[F] {
    val F = Field[F]
  }

	def apply[F: Field](terms: (F, Int)*): Poly[F] =
		new Poly(terms.toList.map({case (c, i) => Term(c, i)}))

}

// Polynomials form a Euclidean Ring
trait PolynomialRing[F] extends EuclideanRing[Poly[F]] {

  implicit def F: Field[F]

  implicit def TR[F: Field] = new TermRing[F] {
  	val F = Field[F]
  }

  def zero = new Poly(List(Term(F.zero, 0)))

  def one = new Poly(List(Term(F.one, 0)))

  def plus(x: Poly[F], y: Poly[F]) : Poly[F] =
  	new Poly((x.terms ++ y.terms).groupBy(_.index).values.toList.map {
  		l => l.foldLeft(Term(F.zero, 0))(_ + _)
  	})

  def negate(x: Poly[F]): Poly[F] =
  	new Poly(x.terms.map(_.unary_-))

  def times(x: Poly[F], y: Poly[F]) : Poly[F] = {
  	val allTerms = x.terms.flatMap(xterm => y.terms.map(_ * xterm))
  	new Poly(allTerms.groupBy(_.index).values.toList.map {
  		l => l.foldLeft(Term(F.zero, 0))(_ + _)
  	})
  }

  def quotMod(x: Poly[F], y: Poly[F]): (Poly[F], Poly[F]) = {
  	require(!y.isZero, "Can't divide by polynomial of zero!")
		def eval(q: List[F], u: List[F], n: Int): (Poly[F], Poly[F]) = {
			val v0 : F = y.coeffs match {
				case Nil => F.zero
				case v::vs => v
			}
			(u == Nil || n < 0) match {
				case true => (new Poly(makeTermsLE(q)), new Poly(makeTermsBE(u)))
				case false => eval((u.head / v0) :: q, 
														zipSum(u, y.coeffs.map(z => (z * (u.head / v0)).unary_-)).tail,
														n - 1)
			}
		}
		eval(Nil, x.coeffs, x.maxOrder - y.maxOrder)
  }

  def quot(x: Poly[F], y: Poly[F]): Poly[F] = quotMod(x, y)._1
  
  def mod(x: Poly[F], y: Poly[F]) : Poly[F] = quotMod(x, y)._2

  def zipSum(x: List[F], y: List[F]): List[F] = 
  	x.zip(y).map { case (a,b) => a + b }

  def makeTermsBE(xs: List[F]): List[Term[F]] = 
  	xs.zip((0 until xs.length).toList.reverse).map({ 
  		case (c, i) => Term(c, i) })

  def makeTermsLE(xs: List[F]): List[Term[F]] = 
  	xs.zip((0 until xs.length).toList).map({ 
  		case (c, i) => Term(c, i) })

  def gcd(x: Poly[F], y: Poly[F]) : Poly[F] = {
  	if(y.isZero && x.isZero) zero else if(y.isZero) x.monic else gcd(y, mod(x, y))
  }

}

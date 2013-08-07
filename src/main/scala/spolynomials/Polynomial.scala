package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._


// Univariate polynomial term
// n.b. for calculus and division by a scalar a Field[C] instance is required.
case class Term[C, E](val coeff: C, val exp: E) {

	def eval(x: C)(implicit cring: Ring[C], 
													conve: ConvertableFrom[E]): C = 
		cring.times(coeff, cring.pow(x, conve.toInt(exp)))

	def isIndexZero(implicit eord: Order[E],
													 ering: Ring[E]): Boolean = 
		eord.eqv(exp, ering.zero)

	def isZero(implicit cord: Order[C],
											cring: Ring[C]): Boolean = 
		cord.eqv(coeff, cring.zero)

	def divideBy(x: C)(implicit cfield: Field[C]): Term[C, E] = 
		Term(cfield.div(coeff, x), exp)

	def der(implicit cring: Ring[C],
									 ering: Ring[E],
									 conve: ConvertableFrom[E]): Term[C, E] = 
		Term(cring.times(coeff, cring.fromInt(conve.toInt(exp))), 
					ering.minus(exp, ering.one))

	def int(implicit cfield: Field[C],
									 ering: Ring[E],
									 conve: ConvertableFrom[E]): Term[C, E] = 
		Term(cfield.div(coeff, cfield.fromInt(conve.toInt(ering.plus(exp, ering.one)))), 
					ering.plus(exp, ering.one))

	def termString(implicit cord: Order[C],
													cring: Ring[C]) = {
		val pm = cord.compare(coeff, cring.zero)
		(coeff, exp) match {
			case (0, i) => ""
			case (1, 0) => if(pm >= 0) s" + 1" else s" - 1"
			case (-1, 1) => s" - x"
			case (1, 1) => s" + x"
			case (c, 1) => if(pm >= 0) s" + ${c}x" else s" - ${c.unary_-}x"
			case (1, i) => if(pm >= 0) s" + x^$i" else s" - x^$i"
			case (c, 0) => if(pm >= 0) s" + ${c}" else s" - ${c.unary_-}"
			case (c, i) => if(pm >= 0) s" + ${c}x^$i" else s" - ${c.unary_-}x^$i"
		}
	}

}


// Univariate polynomial class
class Poly[C: ClassTag, E](val terms: Array[Term[C, E]])
													(implicit eord: Order[E]) {

	implicit object BigEndianPolyOrdering extends Order[Term[C, E]] {
	  def compare(x:Term[C, E], y:Term[C, E]): Int = eord.compare(y.exp, x.exp)
	}

	def allTerms(implicit conve: ConvertableFrom[E],
												cring: Ring[C],
												ering: Ring[E]): Array[Term[C, E]] = {
		QuickSort.sort(terms)
		val cs = new Array[Term[C, E]](conve.toInt(ering.plus(maxOrder, ering.one)))
		terms.foreach(t => cs(conve.toInt(t.exp)) = t)
		for(i <- 0 to conve.toInt(maxOrder)) 
			if(cs(i) == null) cs(i) = Term(cring.zero, ering.fromInt(i))
		cs
	}

	def coeffs(implicit conv: ConvertableFrom[E],
											cring: Ring[C],
											ering: Ring[E]): Array[C] = 
		allTerms.map(_.coeff).toArray

	def maxTerm(implicit cring: Ring[C],
											 ering: Ring[E]): Term[C, E] = {
		if(isZero) Term(cring.zero, ering.zero) else QuickSort.sort(terms); 
		terms(0)
	}

	def maxOrder(implicit cring: Ring[C],
												ering: Ring[E]): E = maxTerm.exp

	def maxOrderTermCoeff(implicit cring: Ring[C],
												ering: Ring[E]): C = maxTerm.coeff

	def degree(implicit cring: Ring[C],
											ering: Ring[E],
											conve: ConvertableFrom[E]): E = 
		if(terms.isEmpty) ering.zero else {
			QuickSort.sort(terms)
			terms.find(_.coeff != cring.zero).getOrElse(Term(cring.zero, ering.zero)).exp
		}
	
	def apply(x: C)(implicit cring: Ring[C],
													 conve: ConvertableFrom[E]): C =
		terms.map(_.eval(x)).foldLeft(cring.zero)(_ + _)

	def isZero: Boolean = terms.isEmpty

	def monic(implicit cfield: Field[C],
										 ering: Ring[E]): Poly[C, E] = 
	 if(isZero) this else new Poly(terms.map(_.divideBy(maxOrderTermCoeff)))
	
	def derivative(implicit cring: Ring[C],
													ering: Ring[E],
													conve: ConvertableFrom[E]): Poly[C, E] = 
		new Poly(terms.filterNot(_.isIndexZero).map(_.der))
	
	def integral(implicit cfield: Field[C],
												ering: Ring[E],
												conve: ConvertableFrom[E]): Poly[C, E] = 
		new Poly(terms.map(_.int))
	
	def show(implicit cord: Order[C],
										cring: Ring[C]) : String = {
		QuickSort.sort(terms)
		val s = terms.map(_.termString).mkString
		if(s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)
	}

}


object Poly {

  implicit def pRDI[C: ClassTag, E]: PolynomialRing[Double, Int] = new PolynomialRing[Double, Int] {
  	val ctc = classTag[Double]
  	val cring = Ring[Double]
  	val ering = Ring[Int]
  	val cord = Order[Double]
  	val eord = Order[Int]
  	val conve = ConvertableFrom[Int]
  	val cfield = Field[Double]
  }

  implicit def pRRI[C: ClassTag, E]: PolynomialRing[Rational, Int] = new PolynomialRing[Rational, Int] {
  	val ctc = classTag[Rational]
  	val cring = Ring[Rational]
  	val ering = Ring[Int]
  	val cord = Order[Rational]
  	val eord = Order[Int]
  	val conve = ConvertableFrom[Int]
  	val cfield = Field[Rational]
  }

}

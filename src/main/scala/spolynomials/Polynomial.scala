package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomial term - No requirements for instantiation whatsoever.
// n.b. for calculus and division by a scalar a Field[C] instance is required.
case class Term[C, E](val coeff: C, val exp: E)
										 (implicit cring: Ring[C],
															 ering: Ring[E],
															 cord: Order[C],
															 eord: Order[E],
															 conve: ConvertableFrom[E],
															 cfield: Field[C]) {

	implicit def tR[C, E]: TermRing[C, E] = new TermRing[C, E] {
		val cring: Ring[C] = this.cring
		val ering: Ring[E] = this.ering
		val cord: Order[C] = this.cord
		val eord: Order[E] = this.eord
		val conve: ConvertableFrom[E] = this.conve
		val cfield: Field[C] = this.cfield
	}

	def eval(x: C): C = 
		cring.times(coeff, cring.pow(x, conve.toInt(exp)))

	def isIndexZero: Boolean = 
		eord.eqv(exp, ering.zero)

	def isZero: Boolean = 
		cord.eqv(coeff, cring.zero)

	def divideBy(x: C): Term[C, E] = 
		Term(cfield.div(coeff, x), exp)

	def der: Term[C, E] = 
		Term(cring.times(coeff, cring.fromInt(conve.toInt(exp))), 
					ering.minus(exp, ering.one))

	def int: Term[C, E] = 
		Term(cfield.div(coeff, cfield.fromInt(conve.toInt(ering.plus(exp, ering.one)))), 
					ering.plus(exp, ering.one))

	def termString = {
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
final class Poly[C: ClassTag, E](val terms: Array[Term[C, E]])
																(implicit cring: Ring[C],
																					ering: Ring[E],
																					cord: Order[C],
																					eord: Order[E],
																					conve: ConvertableFrom[E],
																					cfield: Field[C]) {

	implicit def pR[C: ClassTag, E]: PolynomialRing[C, E] = new PolynomialRing[C, E] {
		val ctc = classTag[C]
		val cring: Ring[C] = this.cring
		val ering: Ring[E] = this.ering
		val cord: Order[C] = this.cord
		val eord: Order[E] = this.eord
		val conve: ConvertableFrom[E] = this.conve
		val cfield: Field[C] = this.cfield
		val tR = new TermRing[C, E] {
			val cring: Ring[C] = this.cring
			val ering: Ring[E] = this.ering
			val cord: Order[C] = this.cord
			val eord: Order[E] = this.eord
			val conve: ConvertableFrom[E] = this.conve
		}
	}

	implicit object BigEndianPolyOrdering extends Order[Term[C, E]] {
	  def compare(x:Term[C, E], y:Term[C, E]): Int = eord.compare(y.exp, x.exp)
	}

	def allTerms: Array[Term[C, E]] = {
		QuickSort.sort(terms)
		val cs = new Array[Term[C, E]](conve.toInt(ering.plus(maxOrder, ering.one)))
		terms.foreach(t => cs(conve.toInt(t.exp)) = t)
		for(i <- 0 to conve.toInt(maxOrder)) 
			if(cs(i) == null) cs(i) = Term(cring.zero, ering.fromInt(i))
		cs
	}

	def coeffs: Array[C] = 
		allTerms.map(_.coeff)

	def maxTerm: Term[C, E] = {
		if(isZero) Term(cring.zero, ering.zero) else QuickSort.sort(terms); 
		terms(0)
	}

	def maxOrder: E = maxTerm.exp

	def maxOrderTermCoeff: C = maxTerm.coeff

	def degree: E = 
		if(terms.isEmpty) ering.zero else {
			QuickSort.sort(terms)
			terms.find(_.coeff != cring.zero).getOrElse(Term(cring.zero, ering.zero)).exp
		}
	
	def apply(x: C): C =
		terms.map(_.eval(x)).foldLeft(cring.zero)(_ + _)

	def isZero: Boolean = terms.isEmpty

	def monic: Poly[C, E] = 
	 if(isZero) this else new Poly(terms.map(_.divideBy(maxOrderTermCoeff)))
	
	def derivative: Poly[C, E] = 
		new Poly(terms.filterNot(_.isIndexZero).map(_.der))
	
	def integral: Poly[C, E] = 
		new Poly(terms.map(_.int))
	
	override def toString = {
		QuickSort.sort(terms)
		checkString(terms.map(_.termString).mkString)
	}

	private def checkString(s: String) : String = 
		if(s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)

}

object Poly {

	def apply[C: ClassTag: Field: Order, E: Ring: Order: ConvertableFrom](terms: Array[Term[C, E]]): Poly[C, E] =
		new Poly(terms)
		
}


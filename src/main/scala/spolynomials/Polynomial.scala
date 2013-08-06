package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomial class
<<<<<<< HEAD
final class Poly[C: ClassTag, E](val terms: Array[Term[C, E]]) extends PolyImplicits {
=======
final class Poly[C: ClassTag, E](val terms: Array[Term[C, E]]) {

	implicit def eord: Order[E] = Order[E] 
>>>>>>> parent of a085e51... Issues with implicit resolution from Interval-like approach

	implicit object BigEndianPolyOrdering extends Order[Term[C, E]] {
	  def compare(x:Term[C, E], y:Term[C, E]): Int = eord.compare(y.exp, x.exp)
	}

	lazy val allTerms: Array[Term[C, E]] = {
		QuickSort.sort(terms)
		val cs = new Array[Term[C, E]](conve.toInt(ering.plus(maxOrder, ering.one)))
		terms.foreach(t => cs(conve.toInt(t.exp)) = t)
		for(i <- 0 to conve.toInt(maxOrder)) 
			if(cs(i) == null) cs(i) = Term(cring.zero, ering.fromInt(i))
		cs
	}

	lazy val coeffs: Array[C] = allTerms.map(_.coeff)

	lazy val maxTerm: Term[C, E] = {
		if(isZero) Term(cring.zero, ering.zero) else QuickSort.sort(terms); 
		terms(0)
	}

	lazy val maxOrder: E = maxTerm.exp

	lazy val maxOrderTermCoeff: C = maxTerm.coeff

	lazy val degree: E = 
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
		implicit def eord: Order[C] = Order[C]
		implicit def cring: Ring[C] = Ring[C]
		QuickSort.sort(terms)
		checkString(terms.map(_.termString).mkString)
	}

	private def checkString(s: String) : String = 
		if(s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)

}

trait PolyImplicits {

	implicit def ctc: ClassTag[C]
  implicit def cring: Ring[C]
  implicit def ering: Ring[E]
  implicit def cord: Order[C]
  implicit def eord: Order[E]
  implicit def conve: ConvertableFrom[E]
  implicit def cfield: Field[C]

}


trait PolyRings {

<<<<<<< HEAD
	implicit def tR[C: Ring, E: Ring]: TermRing[C, E] = new TermRing[C, E] {
		val cring = Ring[C]
		val ering = Ring[E]
		val cord = Order[C]
	}

	implicit def polyRing[C: Field: ClassTag, E: Ring: ConvertableFrom]: PolynomialRing[C, E] = new PolynomialRing[C, E] {
		val tR = new TermRing[C, E] {
			val cring = Ring[C]
			val ering = Ring[E]
			val cord = Order[C]
		}
		val ctc = classTag[C]
		val cring = Ring[C]
		val ering = Ring[E]
		val cord = Order[C]
		val eord = Order[E]

		val conve = ConvertableFrom[E]
		val cfield = Field[C]
	}

}

// Companion object for Poly
object Poly extends PolyRings {
=======
	implicit object tR extends TermRing[C, E] {}

	// implicit def ord[X]: Order[X] = Order[X]
	// implicit def ring[X]: Ring[X] = Ring[X]
	// implicit def conv[X]: ConvertableFrom[X] = ConvertableFrom[X]
	// implicit def termRing[C, E]: TermRing[C, E] = new TermRing[C, E] {}
	// implicit def polyRing[C: ClassTag, E] = new PolynomialRing[C, E] {
	//   val ctc: ClassTag[C] = classTag[C]
	//   val termRing = new TermRing[C, E] {}
 //  }
>>>>>>> parent of a085e51... Issues with implicit resolution from Interval-like approach

  def apply[C: Field: ClassTag, E: Ring: ConvertableFrom](terms: Array[Term[C, E]]): Poly[C, E] =
  	new Poly(terms)
 
}

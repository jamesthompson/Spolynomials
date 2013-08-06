package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomial class
final class Poly[C: ClassTag, E](val terms: Array[Term[C, E]])
																(implicit eord: Order[E],
																					cord: Order[C],
																					cring: Ring[C],
																					ering: Ring[E],
																					cfield: Field[C],
																					conve: ConvertableFrom[E]) {

	implicit def BigEndianPolyOrdering = new Order[Term[C, E]] {
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

// Companion object for Poly
// object Poly {

// 	implicit def tR[C: Ring, E: Ring: Order]: TermRing[C, E] = new TermRing[C, E] {
// 		val cring = Ring[C]
// 		val ering = Ring[E]
// 		val eord = Order[E]
// 	}

// 	implicit def pR[C: Field: ClassTag: Order, E: Ring: Order: ConvertableFrom]: PolynomialRing[C, E] = 
// 		new PolynomialRing[C, E] {
// 			val cring = Ring[C]
// 			val ering = Ring[E]
// 			val cord = Order[C]
// 			val eord = Order[E]
// 			val cfield = Field[C]
// 			val conve = ConvertableFrom[E]
// 			val tR = new TermRing[C, E] {
// 				val cring = Ring[C]
// 				val ering = Ring[E]
// 			}
// 			val ctc = classTag[C]
// 		}


//   // def apply[C: ClassTag, E](terms: Array[Term[C, E]]): Poly[C, E] =
//   // 	new Poly(terms)
 
// }

package spolynomials

import scala.reflect.ClassTag
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomial class
final class Poly[C: ClassTag, E](val terms: Array[Term[C, E]]) {

	implicit def eord: Order[E] = Order[E]

	implicit def tR: TermRing[C, E] = new TermRing[C, E] {} // PROBLEM HERE...

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
		allTerms.map(_.coeff)

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
	
	override def toString = {
		implicit def eord: Order[C] = Order[C]
		implicit def cring: Ring[C] = Ring[C]
		QuickSort.sort(terms)
		checkString(terms.map(_.termString).mkString)
	}

	private def checkString(s: String) : String = 
		if(s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)

}

// Companion object for Poly
object Poly {


 //  def apply[C: ClassTag, E](terms: Array[Term[C, E]]): Poly[C, E] =
 //  	new Poly(terms)
 
}

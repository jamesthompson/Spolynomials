package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomial class
final class Poly[C, E](val terms: Array[Term[C, E]]) {

	implicit def ord = new Order[E] {}

	implicit object BigEndianPolyOrdering extends Order[Term[C, E]] {
	  def compare(x:Term[C, E], y:Term[C, E]): Int = ord.compare(y.exp, x.exp)
	}

	def coeffs(implicit cring: Ring[C]): Array[C] = {
		QuickSort.sort(terms)
		val cs = new Array[C](maxOrder + 1)
		terms.foreach(t => cs(t.exp) = t.coeff)
		for(i <- 0 to maxOrder) if(cs(i) == null) cs(i) = cring.zero
		cs
	}

	def maxTerm(implicit cring: Ring[C], ering: Ring[E]): Term[C, E] = isZero match {
		case true => Term(cring.zero, ering.zero)
		case false => terms.min 
	}

	lazy val maxOrder: Int = terms.min.exp

	lazy val maxOrderTermCoeff: C = terms.min.coeff

	def degree(implicit cring: Ring[C], ering: Ring[E], cord: Order[C]): E = 
		if(terms.isEmpty) ering.zero else {
			QuickSort.sort(terms)
			terms.toList.find(_.coeff != cring.zero).getOrElse(Term(cring.zero, ering.zero)).exp
		}
	
	def apply(x: C)(implicit cring: Ring[C]): C =
		terms.map(_.eval(x)).foldLeft(cring.zero)(_ + _)

	def isZero: Boolean = terms.toList.isEmpty

	def monic: Poly[C, E] = isZero match {
		case true => this
		case false => new Poly(terms.map(_.divideBy(maxOrderTermCoeff)))
	}
	
	def derivative: Poly[C, E] = new Poly(terms.toList.filterNot(_.isIndexZero).map(_.der).toArray)
	
	def integral: Poly[C, E] = new Poly(terms.map(_.int))
	
	override def toString = {
		QuickSort.sort(terms)
		checkString(terms.mkString(""))
	}

	private def checkString(s: String) : String = 
		if(s.take(3) == " - ") ("-".toList :: s.drop(3).toList).toString else s.drop(3)

}

// Companion object for Poly
object Poly {

	implicit def ring[F: Field] = new PolynomialRing[F] {
    val F = Field[F]
  }

	def apply[F: Field](terms: (F, Int)*): Poly[F] = {
		val checkedTerms = terms.filter({case (c, i) => c != 0}).toArray
		new Poly(checkedTerms.map({case (c,i) => Term(c, i)}))
	}

	def fromList[F: Field](terms: List[(F, Int)]): Poly[F] = {
		val checkedTerms = terms.filter({case (c, i) => c != 0})
		new Poly(checkedTerms.map({case (c,i) => Term(c, i)}).toArray)
	}

}

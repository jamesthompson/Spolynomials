package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomial class
final class Poly[C: ClassTag, E](val terms: Array[Term[C, E]])
																(implicit cord: Order[C],
																  				eord: Order[E],
																				  cring: Ring[C],
																				  ering: Ring[E],
																				  cfield: Field[C],
																				  conve: ConvertableFrom[E]) {

	implicit def polyRing = new PolynomialRing[C, E] {
    val ctc = classTag[C]
  }

	implicit object BigEndianPolyOrdering extends Order[Term[C, E]] {
	  def compare(x:Term[C, E], y:Term[C, E]): Int = eord.compare(y.exp, x.exp)
	}

	lazy val coeffs: Array[C] = {
		QuickSort.sort(terms)
		val cs = new Array[C](conve.toInt(ering.plus(maxOrder, ering.one)))
		terms.foreach(t => cs(conve.toInt(t.exp)) = t.coeff)
		for(i <- 0 to conve.toInt(maxOrder)) if(cs(i) == null) cs(i) = cring.zero
		cs
	}

	lazy val maxTerm: Term[C, E] = isZero match {
		case true => Term(cring.zero, ering.zero)
		case false => QuickSort.sort(terms); terms(0)
	}

	lazy val maxOrder: E = maxTerm.exp

	lazy val maxOrderTermCoeff: C = maxTerm.coeff

	def degree: E = 
		if(terms.isEmpty) ering.zero else {
			QuickSort.sort(terms)
			terms.find(_.coeff != cring.zero).getOrElse(Term(cring.zero, ering.zero)).exp
		}
	
	def apply(x: C): C =
		terms.map(_.eval(x)).foldLeft(cring.zero)(_ + _)

	def isZero: Boolean = terms.isEmpty

	def monic: Poly[C, E] = isZero match {
		case true => this
		case false => new Poly(terms.map(_.divideBy(maxOrderTermCoeff)))
	}
	
	def derivative: Poly[C, E] = 
		new Poly(terms.filterNot(_.isIndexZero).map(_.der))
	
	def integral: Poly[C, E] = 
		new Poly(terms.map(_.int))
	
	override def toString = {
		QuickSort.sort(terms)
		checkString(terms.mkString)
	}

	private def checkString(s: String) : String = 
		if(s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)

}

// Companion object for Poly
// object Poly {

// 	implicit def ring[F: Field] = new PolynomialRing[F] {
//     val F = Field[F]
//   }

// 	def apply[F: Field](terms: (F, Int)*): Poly[F] = {
// 		val checkedTerms = terms.filter({case (c, i) => c != 0}).toArray
// 		new Poly(checkedTerms.map({case (c,i) => Term(c, i)}))
// 	}

// 	def fromList[F: Field](terms: List[(F, Int)]): Poly[F] = {
// 		val checkedTerms = terms.filter({case (c, i) => c != 0})
// 		new Poly(checkedTerms.map({case (c,i) => Term(c, i)}).toArray)
// 	}

// }

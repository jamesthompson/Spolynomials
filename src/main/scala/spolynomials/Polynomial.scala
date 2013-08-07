package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._


// Univariate polynomial term
// n.b. for calculus and division by a scalar a Field[C] instance is required.
case class Term[C, E](coeff: C, exp: E) {

  def eval(x: C)(implicit cring: Ring[C], conve: ConvertableFrom[E]): C =
    coeff * (x pow exp.toInt)

  def isIndexZero(implicit eord: Order[E], ering: Ring[E]): Boolean =
    exp === ering.zero

  def isZero(implicit cord: Order[C], cring: Ring[C]): Boolean =
    coeff === cring.zero

  def divideBy(x: C)(implicit cfield: Field[C]): Term[C, E] =
    Term(coeff / x, exp)

  def der(implicit cring: Ring[C], ering: Ring[E], conve: ConvertableFrom[E]): Term[C, E] =
    Term(coeff * cring.fromInt(exp.toInt), exp - ering.one)

  def int(implicit cfield: Field[C], ering: Ring[E], conve: ConvertableFrom[E]): Term[C, E] =
    Term(coeff / cfield.fromInt((exp + ering.one).toInt), exp + ering.one)

  def termString(implicit cord: Order[C], cring: Ring[C]) = {
    val pm = coeff compare cring.zero
    (coeff, exp) match {
      case (0, i) => ""
      case (1, 0) => if (pm >= 0) " + 1" else " - 1"
      case (-1, 1) => " - x"
      case (1, 1) => " + x"
      case (c, 1) => if (pm >= 0) s" + ${c}x" else s" - ${c.unary_-}x"
      case (1, i) => if (pm >= 0) s" + x^$i" else s" - x^$i"
      case (c, 0) => if (pm >= 0) s" + ${c}" else s" - ${c.unary_-}"
      case (c, i) => if (pm >= 0) s" + ${c}x^$i" else s" - ${c.unary_-}x^$i"
    }
  }
}



// Univariate polynomial class
class Poly[C: ClassTag, E](val terms: Array[Term[C, E]])(implicit eord: Order[E]) {

  implicit object BigEndianPolyOrdering extends Order[Term[C, E]] {
    def compare(x:Term[C, E], y:Term[C, E]): Int = y.exp compare x.exp
  }

  def allTerms(implicit conve: ConvertableFrom[E], cring: Ring[C], ering: Ring[E]): Array[Term[C, E]] = {
    QuickSort.sort(terms)
    val m = maxOrder.toInt
    val cs = new Array[Term[C, E]](m + 1)
    terms.foreach(t => cs(t.exp.toInt) = t)
    for(i <- 0 to m)
      if (cs(i) == null) cs(i) = Term(cring.zero, ering.fromInt(i))
    cs
  }

  def coeffs(implicit conv: ConvertableFrom[E], cring: Ring[C], ering: Ring[E]): Array[C] = 
    allTerms.map(_.coeff).toArray

  def maxTerm(implicit cring: Ring[C], ering: Ring[E]): Term[C, E] =
    if (isZero) Term(cring.zero, ering.zero) else terms.qmin

  def maxOrder(implicit cring: Ring[C], ering: Ring[E]): E =
    maxTerm.exp

  def maxOrderTermCoeff(implicit cring: Ring[C], ering: Ring[E]): C =
    maxTerm.coeff

  def degree(implicit cring: Ring[C], eqv: Eq[C], ering: Ring[E], conve: ConvertableFrom[E]): E = {
    val ts = terms.filter(_.coeff =!= cring.zero)
    if (ts.isEmpty) ering.zero else ts.qmin.exp
  }
	
  def apply(x: C)(implicit cring: Ring[C], conve: ConvertableFrom[E]): C =
    terms.map(_.eval(x)).qsum

  def isZero: Boolean = terms.isEmpty

  def monic(implicit cfield: Field[C], ering: Ring[E]): Poly[C, E] = 
    if (isZero) this else new Poly(terms.map(_.divideBy(maxOrderTermCoeff)))
	
  def derivative(implicit cring: Ring[C], ering: Ring[E], conve: ConvertableFrom[E]): Poly[C, E] = 
    new Poly(terms.filterNot(_.isIndexZero).map(_.der))
	
  def integral(implicit cfield: Field[C], ering: Ring[E], conve: ConvertableFrom[E]): Poly[C, E] = 
    new Poly(terms.map(_.int))
	
  def show(implicit cord: Order[C], cring: Ring[C]) : String = {
    QuickSort.sort(terms)
    val s = terms.map(_.termString).mkString
    if(s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)
  }
}


object Poly {

  implicit def pRDI: PolynomialRing[Double, Int] = new PolynomialRing[Double, Int] {
    val ctc = classTag[Double]
    val cring = Ring[Double]
    val ering = Ring[Int]
    val cord = Order[Double]
    val eord = Order[Int]
    val conve = ConvertableFrom[Int]
    val cfield = Field[Double]
  }

  implicit def pRRI: PolynomialRing[Rational, Int] = new PolynomialRing[Rational, Int] {
    val ctc = classTag[Rational]
    val cring = Ring[Rational]
    val ering = Ring[Int]
    val cord = Order[Rational]
    val eord = Order[Int]
    val conve = ConvertableFrom[Int]
    val cfield = Field[Rational]
  }
}

package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._


// Univariate polynomial term
// n.b. for calculus and division by a scalar a Field[C] instance is required.
case class Term[C](coeff: C, exp: Int) {

  def eval(x: C)(implicit cring: Ring[C]): C =
    coeff * (x pow exp)

  def isIndexZero: Boolean =
    exp == 0

  def isZero(implicit ceq: Eq[C], cring: Ring[C]): Boolean =
    coeff === cring.zero

  def divideBy(x: C)(implicit cfield: Field[C]): Term[C] =
    Term(coeff / x, exp)

  def der(implicit cring: Ring[C]): Term[C] =
    Term(coeff * cring.fromInt(exp), exp - 1)

  def int(implicit cfield: Field[C]): Term[C] =
    Term(coeff / cfield.fromInt(exp + 1), exp + 1)

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
case class Poly[C: ClassTag](data: Map[Int, C]) {

  def terms: Array[Term[C]] = data.map { case (e, c) => Term(c, e) }.toArray

  implicit object BigEndianPolyOrdering extends Order[Term[C]] {
    def compare(x:Term[C], y:Term[C]): Int = y.exp compare x.exp
  }

  def allTerms(implicit cring: Ring[C]): Array[Term[C]] = {
    val ts = terms
    QuickSort.sort(ts)
    val m = maxOrder
    val cs = new Array[Term[C]](m + 1)
    ts.foreach(t => cs(t.exp) = t)
    for(i <- 0 to m)
      if (cs(i) == null) cs(i) = Term(cring.zero, i)
    cs
  }

  def coeffs: Array[C] =
    terms.map(_.coeff)

  def maxTerm(implicit cring: Ring[C]): Term[C] =
    if (isZero) Term(cring.zero, 0) else terms.qmin

  def maxOrder(implicit cring: Ring[C]): Int =
    maxTerm.exp

  def maxOrderTermCoeff(implicit cring: Ring[C]): C =
    maxTerm.coeff

  def degree(implicit cring: Ring[C], eqv: Eq[C]): Int = {
    val ts = terms.filter(_.coeff =!= cring.zero)
    if (ts.isEmpty) 0 else ts.qmin.exp
  }
	
  def apply(x: C)(implicit cring: Ring[C]): C =
    terms.map(_.eval(x)).qsum

  //def isZero: Boolean = terms.forall(_.isZero)
  def isZero = terms.isEmpty

  def monic(implicit cfield: Field[C]): Poly[C] = 
    if (isZero) this else Poly(terms.map(_.divideBy(maxOrderTermCoeff)))
	
  def derivative(implicit cring: Ring[C]): Poly[C] = 
    Poly(terms.filterNot(_.isIndexZero).map(_.der))
	
  def integral(implicit cfield: Field[C]): Poly[C] = 
    Poly(terms.map(_.int))
	
  def show(implicit cord: Order[C], cring: Ring[C]) : String = {
    val ts = terms
    QuickSort.sort(ts)
    val s = ts.map(_.termString).mkString
    if(s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)
  }
}


object Poly {

  def apply[C: ClassTag](arr: Array[Term[C]]): Poly[C] =
    Poly(arr.map { case Term(c, e) => (e, c) }.toMap)

  implicit def pRDI: PolynomialRing[Double] = new PolynomialRing[Double] {
    val ctc = classTag[Double]
    val cring = Ring[Double]
    val cord = Order[Double]
    val cfield = Field[Double]
  }

  implicit def pRRI: PolynomialRing[Rational] = new PolynomialRing[Rational] {
    val ctc = classTag[Rational]
    val cring = Ring[Rational]
    val cord = Order[Rational]
    val cfield = Field[Rational]
  }
}

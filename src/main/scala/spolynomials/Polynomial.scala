package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._


// Univariate polynomial term
// n.b. for calculus and division by a scalar a Field[C] instance is required.
case class Term[C](coeff: C, exp: Int) {

  def toTuple: (Int, C) = (exp, coeff)

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
    import cring._
    (coeff, exp) match {
      case (0, _) => ""
      case (1, i) => if (i > 0) s" + x^$i" else s" + x"
      case (-1, i) => if (i > 0) s" - x^$i" else s" - x"
      case (c, 0) => if (c >= zero) s" + ${c}" else s" - ${-c}"
      case (c, 1) => if (c >= zero) s" + ${c}x" else s" - ${-c}x"
      case (c, i) => if (c >= zero) s" + ${c}x^$i" else s" - ${-c}x^$i"
    }
  }
}

object Term {
  def fromTuple[C](tpl: (Int, C)): Term[C] = Term(tpl._2, tpl._1)
  def zero[C](implicit cring: Ring[C]): Term[C] = Term(cring.zero, 0)
  def one[C](implicit cring: Ring[C]): Term[C] = Term(cring.one, 0)
}

// Univariate polynomial class
case class Poly[C: ClassTag](data: Map[Int, C]) {

  def terms: Array[Term[C]] =
    data.map(Term.fromTuple).toArray

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
    data.values.toArray

  def maxTerm(implicit cring: Ring[C]): Term[C] =
    data.foldLeft(Term.zero[C]) { case (term, (e, c)) =>
      if (term.exp < e) Term(c, e) else term
    }

  def maxOrder(implicit cring: Ring[C]): Int =
    if (data.isEmpty) 0 else data.keys.qmax

  def maxOrderTermCoeff(implicit cring: Ring[C]): C =
    maxTerm.coeff

  def degree(implicit cring: Ring[C], eqv: Eq[C]): Int =
    data.foldLeft(0) { case (d, (e, c)) =>
      if (e > d && c =!= cring.zero) e else d
    }
	
  def apply(x: C)(implicit cring: Ring[C]): C =
    data.foldLeft(cring.zero)((sum, t) => sum + Term.fromTuple(t).eval(x))

  def isZero(implicit cring: Ring[C], eqv: Eq[C]): Boolean =
    data.forall { case (e, c) => c === cring.zero }

  def monic(implicit cfield: Field[C]): Poly[C] = {
    val m = maxOrderTermCoeff
    Poly(data.map { case (e, c) => (e, c / m) })
  }
	
  def derivative(implicit cring: Ring[C]): Poly[C] =
    Poly(data.flatMap { case (e, c) =>
      if (e > 0) Some(Term(c, e).der) else None
    })
	
  def integral(implicit cfield: Field[C]): Poly[C] =
    Poly(data.map(t => Term.fromTuple(t).int))
	
  def show(implicit cord: Order[C], cring: Ring[C]) : String =
    if (isZero) {
      "(0)"
    } else {
      val ts = terms
      QuickSort.sort(ts)
      val s = ts.map(_.termString).mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }
}


object Poly {

  def apply[C: ClassTag](terms: Iterable[Term[C]]): Poly[C] =
    Poly(terms.map(_.toTuple).toMap)

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

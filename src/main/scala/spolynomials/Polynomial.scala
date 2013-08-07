package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._


// Univariate polynomial term
// n.b. for calculus and division by a C, a Field[C] instance is required.
case class Term[C](coeff: C, exp: Long) {

  def toTuple: (Long, C) = (exp, coeff)

  def eval(x: C)(implicit r: Ring[C]): C =
    coeff * (x pow exp.intValue)

  def isIndexZero: Boolean = 
    exp == 0

  def isZero(implicit eq: Eq[C], r: Ring[C]): Boolean =
    coeff === r.zero

  def divideBy(x: C)(implicit f: Field[C]): Term[C] =
    Term(coeff / x, exp)

  def der(implicit r: Ring[C]): Term[C] =
    Term(coeff * r.fromInt(exp.intValue), exp - 1)

  def int(implicit f: Field[C]): Term[C] =
    Term(coeff / f.fromInt((exp + 1).intValue), exp + 1)

  def termString(implicit o: Order[C], r: Ring[C]) = {
    import r._
    (coeff, exp.intValue) match {
      case (0, _) => ""
      case (c, 0) => if (c >= zero) s" + ${c}" else s" - ${-c}"
      case (1, e) => if (e > 1) s" + x^$e" else s" + x"
      case (-1, e) => if (e > 1) s" - x^$e" else s" - x"
      case (c, 1) => if (c >= zero) s" + ${c}x" else s" - ${-c}x"
      case (c, e) => if (c >= zero) s" + ${c}x^$e" else s" - ${-c}x^$e"
    }
  }
}

object Term {
  def fromTuple[C](tpl: (Long, C)): Term[C] = Term(tpl._2, tpl._1)
  def zero[C](implicit r: Ring[C]): Term[C] = Term(r.zero, 0L)
  def one[C](implicit r: Ring[C]): Term[C] = Term(r.one, 0L)
}

// Univariate polynomial class
case class Polynomial[C: ClassTag](data: Map[Long, C]) {

  def terms: Array[Term[C]] =
    data.map(Term.fromTuple).toArray

  implicit object BigEndianPolynomialOrdering extends Order[Term[C]] {
    def compare(x:Term[C], y:Term[C]): Int = y.exp compare x.exp
  }

  def allTerms(implicit r: Ring[C]): Array[Term[C]] = {
    val ts = terms
    QuickSort.sort(ts)
    val m = maxOrder
    val cs = new Array[Term[C]]((m + 1).intValue)
    ts.foreach(t => cs(t.exp.intValue) = t)
    for(i <- 0 to m.intValue)
      if (cs(i) == null) cs(i) = Term(r.zero, i)
    cs
  }

  def coeffs: Array[C] =
    data.values.toArray

  def maxTerm(implicit r: Ring[C]): Term[C] =
    data.foldLeft(Term.zero[C]) { case (term, (e, c)) =>
      if (term.exp < e) Term(c, e) else term
    }

  def maxOrder(implicit r: Ring[C]): Long =
    if (data.isEmpty) 0 else data.keys.qmax

  def maxOrderTermCoeff(implicit r: Ring[C]): C =
    maxTerm.coeff

  def degree(implicit r: Ring[C], eq: Eq[C]): Long =
    data.foldLeft(0) { case (d, (e, c)) =>
      if (e > d && c =!= r.zero) e.intValue else d
    }
	
  def apply(x: C)(implicit r: Ring[C]): C =
    data.foldLeft(r.zero)((sum, t) => sum + Term.fromTuple(t).eval(x))

  def isZero(implicit r: Ring[C], eq: Eq[C]): Boolean =
    data.forall { case (e, c) => c === r.zero }

  def monic(implicit f: Field[C]): Polynomial[C] = {
    val m = maxOrderTermCoeff
    Polynomial(data.map { case (e, c) => (e, c / m) })
  }
	
  def derivative(implicit r: Ring[C]): Polynomial[C] =
    Polynomial(data.flatMap { case (e, c) =>
      if (e > 0) Some(Term(c, e).der) else None
    })
	
  def integral(implicit f: Field[C]): Polynomial[C] =
    Polynomial(data.map(t => Term.fromTuple(t).int))
	
  def show(implicit o: Order[C], r: Ring[C]) : String =
    if (isZero) {
      "(0)"
    } else {
      val ts = terms
      QuickSort.sort(ts)
      val s = ts.map(_.termString).mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }
}


object Polynomial {

  def apply[C: ClassTag](terms: Iterable[Term[C]]): Polynomial[C] =
    Polynomial(terms.map(_.toTuple).toMap)

  implicit def pRD: PolynomialRing[Double] = new PolynomialRing[Double] {
    val ct = classTag[Double]
    val r = Ring[Double]
    val o = Order[Double]
    val f = Field[Double]
  }

  implicit def pRR: PolynomialRing[Rational] = new PolynomialRing[Rational] {
    val ct = classTag[Rational]
    val r = Ring[Rational]
    val o = Order[Rational]
    val f = Field[Rational]
  }
}

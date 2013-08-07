package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._

object BasicPolynomialTesting extends App {

  // val a: Polynomial[Rational] = Polynomial(Array(Term(r"1/100", 1), Term(r"2/1", 3), Term(r"-17/19", 2)))
  // val b: Polynomial[Rational] = Polynomial(Array(Term(r"1/2", 1), Term(r"-3/2", 3), Term(r"1/4", 2)))

  val p1 = Polynomial(Array(Term(r"2/1", 2), Term(r"1/1", 1)))
  val p2 = Polynomial(Array(Term(r"1/3", 2), Term(r"2/1", 1)))

  // println(a.show)
  // println(b.show)
  // println((a + b).show)
  // println((b * a).show)


  val d = p1 % p2
  val e = p1 /~ p2

  println(d.show)
  println(e.show)
  // println(p1.maxOrderTermCoeff)
  // println(p1.maxOrder)
  // println(p1.coeffs.mkString("\t"))

  // println(a(r"2/1"))
  // println(a.derivative.show)
  // println(a.monic.show)

  // val leg = SpecialPolynomials.legendres(10).map(_.show).mkString("\n")
  // println(leg)

  // println(Polynomial(Map(0L -> Numeric[Rational].one)).show)
}

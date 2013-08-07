package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._

object BasicPolynomialTesting extends App {

  val a: Poly[Rational] = Poly(Array(Term(r"1/100", 1), Term(r"2/1", 3), Term(r"-17/19", 2)))
  val b: Poly[Rational] = Poly(Array(Term(r"1/2", 1), Term(r"-3/2", 3), Term(r"1/4", 2)))

  val p1 = Poly(Array(Term(r"2/1", 2), Term(r"1/1", 1)))
  val p2 = Poly(Array(Term(r"1/3", 2), Term(r"2/1", 1)))

  println(a.show)
  println(b.show)

  val d = p1 % p2
  val e = p1 /~ p2

  println(d.show)
  println(e.show)

  println(a(r"2/1"))
  println(a.derivative.show)
  println(a.monic.show)
}

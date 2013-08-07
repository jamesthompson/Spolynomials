package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._

object BasicPolynomialTesting extends App {


  val a: Polynomial[Rational] = Polynomial(Array(Term(r"9/1", 10L), Term(r"3/1", 3), Term(r"2/1", 1), Term(r"14/1", 0)))
  val b: Polynomial[Rational] = Polynomial(Array(Term(r"6/1", 6L), Term(r"9/1", 5), Term(r"4/1", 0)))

  val c = a /~ b
  println(c.show)

}

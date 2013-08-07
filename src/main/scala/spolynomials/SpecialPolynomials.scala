package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

object SpecialPolynomials {

	// Returns a list of legendre polynomials (Rational kind) up to i
	def legendres(i: Int)(implicit r: Numeric[Rational], pr: PolynomialRing[Rational]) : List[Polynomial[Rational]] = {
	  val one = Polynomial(Map(0L -> r.one))
	  val x = Polynomial(Map(1L -> r.one))
	  lazy val leg : Stream[Polynomial[Rational]] = {
	    def loop(pnm1: Polynomial[Rational], pn: Polynomial[Rational], n: Int = 1) : Stream[Polynomial[Rational]] = {
	      pn #:: loop(pn, Polynomial(Map(0L -> r.fromInt(n + 1).reciprocal)) * (
	                 (Polynomial(Map(1L -> r.fromInt(2 * n + 1))) * pn ) + 
	                 (Polynomial(Map(0L -> r.fromInt(n).unary_-)) * pnm1)), n + 1)
	    }
	    one #:: loop(one, x)
	  }
	  leg.take(i).toList
	}

}
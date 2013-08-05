package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

object SpecialPolynomials {

  // // Returns a list of legendre polynomials (Rational kind) up to i
  // def legendres(i: Int)(implicit R: Numeric[Rational]) : List[Poly[Rational]] = {
  //   val one = Poly(R.one -> 0)
  //   val x = Poly(R.one -> 1)
  //   lazy val leg : Stream[Poly[Rational]] = {
  //     def loop(pnm1: Poly[Rational], pn: Poly[Rational], n: Int = 1) : Stream[Poly[Rational]] = {
  //       pn #:: loop(pn, Poly(R.fromInt(n + 1).reciprocal -> 0) * (
  //                  (Poly(R.fromInt(2 * n + 1) -> 1) * pn ) + 
  //                  (Poly(R.fromInt(n).unary_- -> 0) * pnm1)), n + 1)
  //     }
  //     one #:: loop(one, x)
  //   }
  //   leg.take(i).toList
  // }

}


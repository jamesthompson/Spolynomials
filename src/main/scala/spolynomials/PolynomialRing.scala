package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomials form a Euclidean Ring
trait PolynomialRing[F] extends EuclideanRing[Poly[F]] {

  implicit def F: Field[F]

  implicit def TR[F: Field] = new TermRing[F] {
  	val F = Field[F]
  }

  def zero = Poly.fromList(Nil)

  def one = Poly(F.one -> 0)

  def plus(x: Poly[F], y: Poly[F]): Poly[F] =
  	Poly((x.terms ++ y.terms).groupBy(_.index).values.toArray.map {
  		l => l.foldLeft(Term(F.zero, 0))(_ + _)
  	})

  def negate(x: Poly[F]): Poly[F] =
  	new Poly(x.terms.map(_.unary_-))

  def times(x: Poly[F], y: Poly[F]) : Poly[F] = {
  	val allTerms = x.terms.flatMap(xterm => y.terms.map(_ * xterm))
  	new Poly(allTerms.groupBy(_.index).values.toArray.map {
  		l => l.foldLeft(Term(F.zero, 0))(_ + _)
  	})
  }

  def quotMod(x: Poly[F], y: Poly[F]): (Poly[F], Poly[F]) = {
    require(!y.isZero, "Can't divide by polynomial of zero!")
    
    def zipSum(x: List[F], y: List[F]): Poly[F] = {
      val (s, l) = if(x.length > y.length) (y, x) else (x, y)
      val cs = s.zip(l).map(z => z._1 + z._2) ++ l.drop(s.length)
      Poly.fromList(cs.zip(((cs.length - 1) to 0 by -1)).tail)
    }

    def polyFromCoeffsLE(cs: List[F]): Poly[F] =
      Poly.fromList(cs.zip((0 until cs.length)))
    
    def eval(q: List[F], u: Poly[F], n: Int): (Poly[F], Poly[F]) = {
      lazy val q0 = u.maxTerm.coeff / y.maxTerm.coeff
      lazy val uprime = zipSum(u.coeffs, y.coeffs.map(_ * q0.unary_-))
      if(u.isZero || n < 0) (polyFromCoeffsLE(q), u) else eval(q0 :: q, uprime, n - 1)
    }
    
    eval(Nil, x, x.degree - y.degree)
  }

  def quot(x: Poly[F], y: Poly[F]): Poly[F] = quotMod(x, y)._1
  
  def mod(x: Poly[F], y: Poly[F]): Poly[F] = quotMod(x, y)._2

  def gcd(x: Poly[F], y: Poly[F]): Poly[F] = 
  	if(y.isZero && x.isZero) zero else if(y.isZero) x.monic
      else gcd(y, mod(x, y))

}

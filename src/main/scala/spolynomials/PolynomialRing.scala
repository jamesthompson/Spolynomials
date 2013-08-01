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

  def zero = new Poly(Nil)

  def one = new Poly(List(Term(F.one, 0)))

  def plus(x: Poly[F], y: Poly[F]): Poly[F] =
  	new Poly((x.terms ++ y.terms).groupBy(_.index).values.toList.map {
  		l => l.foldLeft(Term(F.zero, 0))(_ + _)
  	})

  def negate(x: Poly[F]): Poly[F] =
  	new Poly(x.terms.map(_.unary_-))

  def times(x: Poly[F], y: Poly[F]) : Poly[F] = {
  	val allTerms = x.terms.flatMap(xterm => y.terms.map(_ * xterm))
  	new Poly(allTerms.groupBy(_.index).values.toList.map {
  		l => l.foldLeft(Term(F.zero, 0))(_ + _)
  	})
  }

  def quotMod(x: Poly[F], y: Poly[F]): (Poly[F], Poly[F]) = {
    require(!y.isZero, "Can't divide by polynomial of zero!")

      def zipSum(x: List[F], y: List[F]): List[F] = 
        x.zip(y).map { case (a,b) => a + b }
    
      def makeTerms(xs: List[F]): List[(F, Int)] = 
        xs.zip(((xs.length - 1) to 0 by -1).toList).map({ 
          case (c, i) => (c, i) })
    
    def eval(q: Poly[F], u: Poly[F], n: Int): (Poly[F], Poly[F]) = {
      val v0 : Term[F] = if(y.isZero) Term(F.zero, 0) else y.maxTerm 
      (u.isZero || n < 0) match {
        case true => (q, u)
        case false => eval(
          new Poly((u.maxTerm.divideBy(v0.coeff)) :: q.terms), 
          Poly.fromList(
            makeTerms(
              zipSum(
                u.coeffs, 
                new Poly(y.terms.map(t => (t * u.maxTerm.divideBy(v0.coeff)).unary_-)).coeffs
              )
            ).tail
          ),
          n - 1
        )
      }
    }
    eval(Poly(F.zero -> 0), x, x.degree - y.degree)
  }


  def quot(x: Poly[F], y: Poly[F]): Poly[F] = quotMod(x, y)._1
  
  def mod(x: Poly[F], y: Poly[F]): Poly[F] = quotMod(x, y)._2

  def gcd(x: Poly[F], y: Poly[F]): Poly[F] = 
  	if(y.isZero && x.isZero) zero else if(y.isZero) x.monic
      else gcd(y, mod(x, y))

}

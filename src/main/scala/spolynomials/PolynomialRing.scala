package spolynomials

import scala.reflect.ClassTag
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomials form a Euclidean Ring
trait PolynomialRing[C, E] extends EuclideanRing[Poly[C, E]] {

  implicit val cord: Order[C] = Order[C]
  implicit val eord: Order[E] = Order[E]
  implicit val cring: Ring[C] = Ring[C]
  implicit val ering: Ring[E] = Ring[E]
  implicit val cfield: Field[C] = Field[C]
  implicit val conve: ConvertableFrom[E] = ConvertableFrom[E]
  implicit val ctc: ClassTag[C]

  implicit def termRing = new TermRing[C, E] {}

  def zero = new Poly(Array(termRing.zero))

  def one = new Poly(Array(termRing.one))

  def plus(x: Poly[C, E], y: Poly[C, E]): Poly[C, E] = zero
  	// Poly((x.terms ++ y.terms).groupBy(_.index).values.toArray.map {
  	// 	l => l.foldLeft(Term(F.zero, 0))(_ + _)
  	// })

  def negate(x: Poly[C, E]): Poly[C, E] = zero
  	// new Poly(x.terms.map(_.unary_-))

  def times(x: Poly[C, E], y: Poly[C, E]) : Poly[C, E] = zero
  // {
  // 	val allTerms = x.terms.flatMap(xterm => y.terms.map(_ * xterm))
  // 	new Poly(allTerms.groupBy(_.index).values.toArray.map {
  // 		l => l.foldLeft(Term(F.zero, 0))(_ + _)
  // 	})
  // }

  def quotMod(x: Poly[C, E], y: Poly[C, E]): (Poly[C, E], Poly[C, E]) = {
    // require(!y.isZero, "Can't divide by polynomial of zero!")
    
    // def zipSum(x: List[F], y: List[F]): Poly[F] = {
    //   val (s, l) = if(x.length > y.length) (y, x) else (x, y)
    //   val cs = s.zip(l).map(z => z._1 + z._2) ++ l.drop(s.length)
    //   Poly.fromList(cs.zip(((cs.length - 1) to 0 by -1)).tail)
    // }

    // def polyFromCoeffsLE(cs: List[F]): Poly[F] =
    //   Poly.fromList(cs.zip((0 until cs.length)))
    
    // def eval(q: List[F], u: Poly[F], n: Int): (Poly[F], Poly[F]) = {
    //   lazy val q0 = u.maxTerm.coeff / y.maxTerm.coeff
    //   lazy val uprime = zipSum(u.coeffs, y.coeffs.map(_ * q0.unary_-))
    //   if(u.isZero || n < 0) (polyFromCoeffsLE(q), u) else eval(q0 :: q, uprime, n - 1)
    // }
    
    // eval(Nil, x, x.degree - y.degree)
    (zero, one)
  }

  def quot(x: Poly[C, E], y: Poly[C, E]): Poly[C, E] = quotMod(x, y)._1
  
  def mod(x: Poly[C, E], y: Poly[C, E]): Poly[C, E] = quotMod(x, y)._2

  def gcd(x: Poly[C, E], y: Poly[C, E]): Poly[C, E] = 
  	if(y.isZero && x.isZero) zero else if(y.isZero) x.monic
      else gcd(y, mod(x, y))

}

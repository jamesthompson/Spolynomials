package spolynomials

import scala.reflect.ClassTag
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomials form a Euclidean Ring
trait PolynomialRing[C, E] extends EuclideanRing[Poly[C, E]] {

  implicit def ctc: ClassTag[C]
  implicit def cring: Ring[C]
  implicit def ering: Ring[E]
  implicit def cord: Order[C]
  implicit def eord: Order[E]
  implicit def conve: ConvertableFrom[E]
  implicit def cfield: Field[C]
  implicit def tR: TermRing[C, E]

  def zero = new Poly(Array(tR.zero))

  def one = new Poly(Array(tR.one))

  def negate(x: Poly[C, E]): Poly[C, E] =
    new Poly(x.terms.map(tR.negate))

  def clearZeroesPoly(x: Array[Term[C, E]]): Poly[C, E] =
    new Poly(x.groupBy(_.exp).map({
      case (e, l) => l.foldLeft(tR.zero)(tR.plus(_, _))}).toArray)

  def plus(x: Poly[C, E], y: Poly[C, E]): Poly[C, E] = 
    clearZeroesPoly(x.terms ++ y.terms)

  def times(x: Poly[C, E], y: Poly[C, E]): Poly[C, E] = 
    clearZeroesPoly(x.terms.flatMap { 
      xterm => y.terms.map(yterm => tR.times(yterm, xterm)) })

  def quotMod(x: Poly[C, E], y: Poly[C, E]): (Poly[C, E], Poly[C, E]) = {
    require(!y.isZero, "Can't divide by polynomial of zero!")
    
    def zipSum(x: Array[C], y: Array[C]): Poly[C, E] = {
      val (s, l) = if(x.length > y.length) (y, x) else (x, y)
      val cs = s.zip(l).map(z => cfield.plus(z._1, z._2)) ++ l.drop(s.length)
      new Poly(cs.zip(((cs.length - 1) to 0 by -1)).tail.map({
        case (c, e) => Term(c, ering.fromInt(e))
        }))
    }

    def polyFromCoeffsLE(cs: Array[C]): Poly[C, E] =
      new Poly(cs.zip((0 until cs.length)).map({
        case (c, e) => Term(c, ering.fromInt(e))
        }))
    
    def eval(q: List[C], u: Poly[C, E], n: Int): (Poly[C, E], Poly[C, E]) = {
      lazy val q0 = cfield.div(u.maxOrderTermCoeff, y.maxOrderTermCoeff)
      lazy val uprime = zipSum(u.coeffs, y.coeffs.map(yc => cfield.times(yc, cfield.negate(q0))))
      if(u.isZero || n < 0) (polyFromCoeffsLE(q.toArray), u) else eval(q0 :: q, uprime, n - 1)
    }
    
    eval(Nil, x, conve.toInt(ering.minus(x.degree, y.degree)))
    (zero, one)
  }

  def quot(x: Poly[C, E], y: Poly[C, E]): Poly[C, E] =  quotMod(x, y)._1
  
  def mod(x: Poly[C, E], y: Poly[C, E]): Poly[C, E] =  quotMod(x, y)._2

  def gcd(x: Poly[C, E], y: Poly[C, E]): Poly[C, E] = 
    if(y.isZero && x.isZero) zero else if(y.isZero) x.monic
      else gcd(y, mod(x, y))

}

// Univariate polynomial terms form a ring
trait TermRing[C, E] extends Ring[Term[C, E]] {

  implicit def cring: Ring[C]
  implicit def ering: Ring[E]

  def negate(t: Term[C, E]): Term[C, E] = 
    Term(cring.negate(t.coeff), t.exp)

  def zero: Term[C, E] = 
    Term(cring.zero, ering.zero)

  def one: Term[C, E] = 
    Term(cring.one, ering.zero)

  def plus(x: Term[C, E], y: Term[C, E]): Term[C, E] = 
    Term(cring.plus(x.coeff, y.coeff), y.exp)

  def times(x: Term[C, E], y: Term[C, E]): Term[C, E] = 
    Term(cring.times(x.coeff, y.coeff), ering.plus(x.exp, y.exp))

}

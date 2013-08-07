package spolynomials

import scala.reflect.ClassTag
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.math.compat._

// Univariate Polynomials Form a Ring
trait PolynomialRing[C] extends EuclideanRing[Polynomial[C]] {

  implicit def ct: ClassTag[C]
  implicit def r: Ring[C]
  implicit def o: Order[C]
  implicit def f: Field[C]

  implicit def tR: Ring[Term[C]] = new Ring[Term[C]] {
    def negate(t: Term[C]): Term[C] = Term(-t.coeff, t.exp)
    def zero: Term[C] = Term(r.zero, 0L)
    def one: Term[C] = Term(r.one, 0L)
    def plus(x: Term[C], y: Term[C]): Term[C] =
      Term(x.coeff + y.coeff, y.exp)
    def times(x: Term[C], y: Term[C]): Term[C] =
      Term(x.coeff * y.coeff, x.exp + y.exp)
  }

  def zero = Polynomial(Map(0L -> r.zero))

  def one = Polynomial(Map(0L -> r.one))

  def negate(x: Polynomial[C]): Polynomial[C] =
    Polynomial(x.data.map { case (e, c) => (e, -c) })

  def plus(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    Polynomial(x.data + y.data)

  def times(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    Polynomial(x.data.flatMap { case (ex, cx) =>
      y.data.map { case (ey, cy) => (ex + ey, cx * cy) }
    })

  def quotMod(x: Polynomial[C], y: Polynomial[C]): (Polynomial[C], Polynomial[C]) = {
    require(!y.isZero, "Can't divide by polynomial of zero!")
      
    def zipSum(x: Array[C], y: Array[C]): Polynomial[C] = {
      val (s, l) = if(x.length > y.length) (y, x) else (x, y)
      val cs = s.zip(l).map(z => z._1 + z._2) ++ l.drop(s.length)
      Polynomial(cs.zip(((cs.length - 1) to 0 by -1)).tail.map {
        case (c, e) => Term(c, e)
      })
    }

    def polyFromCoeffsLE(cs: Iterable[C]): Polynomial[C] =
      Polynomial(cs.zipWithIndex.map { case (c, e) => Term(c, e) })
      
    def eval(q: List[C], u: Polynomial[C], n: Long): (Polynomial[C], Polynomial[C]) = {
      lazy val q0 = u.maxOrderTermCoeff / y.maxOrderTermCoeff
      lazy val uprime = zipSum(u.coeffs, y.coeffs.map(_ * -q0))
      if (u.isZero || n < 0) (polyFromCoeffsLE(q), u) else eval(q0 :: q, uprime, n - 1)
    }
      
    eval(Nil, x, x.degree - y.degree)
  }

  def quot(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = quotMod(x, y)._1
    
  def mod(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = quotMod(x, y)._2

  def gcd(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    if (y.isZero && x.isZero) zero
    else if (y.maxTerm.isZero) x
    else gcd(y, mod(x, y))

}

package spolynomials

import scala.reflect.ClassTag
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.math.compat._

// Univariate Polynomials Form a Ring
trait PolynomialRing[C] extends EuclideanRing[Poly[C]] {

  implicit def ctc: ClassTag[C]
  implicit def cring: Ring[C]
  implicit def cord: Order[C]
  implicit def cfield: Field[C]

  implicit def tR: Ring[Term[C]] = new Ring[Term[C]] {
    def negate(t: Term[C]): Term[C] = Term(-t.coeff, t.exp)
    def zero: Term[C] = Term(cring.zero, 0)
    def one: Term[C] = Term(cring.one, 0)
    def plus(x: Term[C], y: Term[C]): Term[C] =
      Term(x.coeff + y.coeff, y.exp) //FIXME
    def times(x: Term[C], y: Term[C]): Term[C] =
      Term(x.coeff * y.coeff, x.exp + y.exp)
  }

  def zero = Poly(Map(0 -> cring.zero))

  def one = Poly(Map(0 -> cring.one))

  def negate(x: Poly[C]): Poly[C] =
    Poly(x.data.map { case (e, c) => (e, -c) })

  def plus(x: Poly[C], y: Poly[C]): Poly[C] =
    Poly(x.data + y.data)

  def times(x: Poly[C], y: Poly[C]): Poly[C] =
    Poly(x.data.flatMap { case (ex, cx) =>
      y.data.map { case (ey, cy) => (ex + ey, cx * cy) }
    })

  def quotMod(x: Poly[C], y: Poly[C]): (Poly[C], Poly[C]) = {
    require(!y.isZero, "Can't divide by polynomial of zero!")
      
    def zipSum(x: Array[C], y: Array[C]): Poly[C] = {
      val (s, l) = if(x.length > y.length) (y, x) else (x, y)
      val cs = s.zip(l).map(z => z._1 + z._2) ++ l.drop(s.length)
      Poly(cs.zip(((cs.length - 1) to 0 by -1)).tail.map {
        case (c, e) => Term(c, e)
      })
    }

    def polyFromCoeffsLE(cs: Iterable[C]): Poly[C] =
      Poly(cs.zipWithIndex.map { case (c, e) => Term(c, e) })
      
    def eval(q: List[C], u: Poly[C], n: Int): (Poly[C], Poly[C]) = {
      lazy val q0 = u.maxOrderTermCoeff / y.maxOrderTermCoeff
      lazy val uprime = zipSum(u.coeffs, y.coeffs.map(_ * -q0))
      if (u.isZero || n < 0) (polyFromCoeffsLE(q), u) else eval(q0 :: q, uprime, n - 1)
    }
      
    eval(Nil, x, x.degree - y.degree)
  }

  def quot(x: Poly[C], y: Poly[C]): Poly[C] = quotMod(x, y)._1
    
  def mod(x: Poly[C], y: Poly[C]): Poly[C] = quotMod(x, y)._2

  def gcd(x: Poly[C], y: Poly[C]): Poly[C] =
    if (y.isZero && x.isZero) zero
    else if (y.maxTerm.isZero) x
    else gcd(y, mod(x, y))
}

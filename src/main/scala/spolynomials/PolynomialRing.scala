package spolynomials

import scala.reflect.ClassTag
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

// Univariate polynomials form a Euclidean Ring
// trait PolynomialRing[C, E] extends EuclideanRing[Poly[C, E]] {

//   implicit def ctc: ClassTag[C]

//   def zero(implicit tR: TermRing[C, E],
//                     cring: Ring[C],
//                     ering: Ring[E]) = new Poly(Array(tR.zero))

//   def one(implicit tR: TermRing[C, E],
//                    cring: Ring[C],
//                    ering: Ring[E]) = new Poly(Array(tR.one))

//   def negate(x: Poly[C, E])(implicit tR: TermRing[C, E],
//                                      cring: Ring[C]): Poly[C, E] =
//     new Poly(x.terms.map(tR.negate))

//   def clearZeroesPoly(x: Array[Term[C, E]])
//                      (implicit tR: TermRing[C, E],
//                                cring: Ring[C],
//                                ering: Ring[E]): Poly[C, E] =
//     new Poly(x.groupBy(_.exp).map({
//       case (e, l) => l.foldLeft(tR.zero)(tR.plus(_, _))}).toArray)

//   def plus(x: Poly[C, E], y: Poly[C, E])
//           (implicit tR: TermRing[C, E],
//                     cring: Ring[C],
//                     ering: Ring[E]): Poly[C, E] = 
//     clearZeroesPoly(x.terms ++ y.terms)

//   def times(x: Poly[C, E], y: Poly[C, E])
//            (implicit tR: TermRing[C, E],
//                      cring: Ring[C],
//                      ering: Ring[E]) : Poly[C, E] = 
//     clearZeroesPoly(x.terms.flatMap { 
//       xterm => y.terms.map(yterm => tR.times(yterm, xterm)) })

//   def quotMod(x: Poly[C, E], y: Poly[C, E])
//              (implicit tR: TermRing[C, E],
//                        cfield: Field[C],
//                        ering: Ring[E],
//                        conve: ConvertableFrom[E]): (Poly[C, E], Poly[C, E]) = {
//     require(!y.isZero, "Can't divide by polynomial of zero!")
    
//     def zipSum(x: Array[C], y: Array[C]): Poly[C, E] = {
//       val (s, l) = if(x.length > y.length) (y, x) else (x, y)
//       val cs = s.zip(l).map(z => cfield.plus(z._1, z._2)) ++ l.drop(s.length)
//       new Poly(cs.zip(((cs.length - 1) to 0 by -1)).tail.map({
//         case (c, e) => Term(c, ering.fromInt(e))
//         }))
//     }

//     def polyFromCoeffsLE(cs: Array[C]): Poly[C, E] =
//       new Poly(cs.zip((0 until cs.length)).map({
//         case (c, e) => Term(c, ering.fromInt(e))
//         }))
    
//     def eval(q: List[C], u: Poly[C, E], n: Int): (Poly[C, E], Poly[C, E]) = {
//       lazy val q0 = cfield.div(u.maxOrderTermCoeff, y.maxOrderTermCoeff)
//       lazy val uprime = zipSum(u.coeffs, y.coeffs.map(yc => cfield.times(yc, cfield.negate(q0))))
//       if(u.isZero || n < 0) (polyFromCoeffsLE(q.toArray), u) else eval(q0 :: q, uprime, n - 1)
//     }
    
//     eval(Nil, x, conve.toInt(ering.minus(x.degree, y.degree)))
//     (zero, one)
//   }

//   def quot(x: Poly[C, E], y: Poly[C, E])
//           (implicit tR: TermRing[C, E],
//                     cfield: Field[C],
//                     ering: Ring[E],
//                     conve: ConvertableFrom[E]): Poly[C, E] =  quotMod(x, y)._1
  
//   def mod(x: Poly[C, E], y: Poly[C, E])
//          (implicit tR: TermRing[C, E],
//                    cfield: Field[C],
//                    ering: Ring[E],
//                    conve: ConvertableFrom[E]): Poly[C, E] =  quotMod(x, y)._2

//   def gcd(x: Poly[C, E], y: Poly[C, E])
//          (implicit tR: TermRing[C, E],
//                    cfield: Field[C],
//                    ering: Ring[E],
//                    conve: ConvertableFrom[E]): Poly[C, E] = 
//   	if(y.isZero && x.isZero) zero else if(y.isZero) x.monic
//       else gcd(y, mod(x, y))

// }

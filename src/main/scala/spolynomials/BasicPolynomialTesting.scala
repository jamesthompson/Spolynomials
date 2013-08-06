package spolynomials

import scala.reflect._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

object BasicPolynomialTesting extends App {

	println("\n\nCompilation successful...\n\n")

	val a: Poly[Double, Int] = new Poly(Array(Term(-0.23, 1), Term(-0.45, 3), Term(0.31, 2)))
	val b: Poly[Double, Int] = new Poly(Array(Term(0.13, 1), Term(-0.25, 3), Term(0.11, 2)))

	println(a.show)
	println(b.show)

	// println(a.unary_-)

	println(a(1.0))
	println(a.derivative.show)
	println(a.monic.show)

	// import spire.syntax.literals._
	// val a = Poly(r"2/1" -> 3, r"1/1" -> 2, r"4/1" -> 0)
	// val b = Poly(r"1/4" -> 3, r"1/10" -> 2, r"3/1" -> 1)

	// val aeval = a(r"2/1")
	// val beval = b(r"1/2")

	// println(s"\na = $a")
	// println(s"b = $b\n")
	// println(s"a + b = ${a + b}")
	// println(s"a - b = ${a - b}")
	// println(s"a * b = ${a * b}")
	// println(s"gcd(a,b) = ${gcd(a,b)}")
	// println(s"a % b = ${a % b}")
	// println(s"a /~ b = ${a /~ b}")
	// println(s"a /% b = ${a /% b}")
	// println(s"a.monic = ${a.monic}")
	// println(s"b.monic = ${b.monic}")
	// println(s"a's derivative = ${a.derivative}")
	// println(s"b's derivative = ${b.derivative}")
	// println(s"a's integral ${a.integral}")
	// println(s"b's integral ${b.integral}")
	// println(s"a evaluated with x = 2/1 = $aeval")
	// println(s"b evaluated with x = 1/2 = $beval")

	// println(s"${SpecialPolynomials.legendres(10)}")

}
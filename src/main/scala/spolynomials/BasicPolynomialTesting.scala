package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

object BasicPolynomialTesting extends App {

	import spire.syntax.literals._
	val a = Poly(r"1/10" -> 3, r"1/20" -> 2, r"2/1" -> 1)
	val b = Poly(r"1/4" -> 3, r"1/10" -> 2, r"3/1" -> 1)

	val absum = a + b
	val abmin = a - b
	val abprod = a * b
	val gcdab = gcd(a, b)
	val amodb = a % b
	val aquotb = a /~ b
	val aquotmodb = a /% b
	val amonic = a.monic
	val bmonic = b.monic
	val ader = a.derivative
	val bder = b.derivative
	val aint = a.integral
	val bint = b.integral
	val aeval = a(r"2/1")
	val beval = b(r"1/2")

	println(s"\na = $a")
	println(s"b = $b\n")
	println(s"a + b = $absum")
	println(s"a - b = $abmin")
	println(s"a * b = $abprod")
	println(s"gcd(a,b) = $gcdab")
	println(s"a % b = $amodb")
	println(s"a /~ b = $aquotb")
	println(s"a /% b = $aquotmodb")
	println(s"a.monic = $amonic")
	println(s"b.monic = $bmonic")
	println(s"a's derivative = $ader")
	println(s"b's derivative = $bder")
	println(s"a's integral $aint")
	println(s"b's integral $bint")
	println(s"a evaluated with x = 2/1 = $aeval")
	println(s"b evaluated with x = 1/2 = $beval")

}
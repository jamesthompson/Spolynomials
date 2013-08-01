package spolynomials

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

object BasicPolynomialTesting extends App {

	import spire.syntax.literals._
	val a = Poly(r"1/100" -> 3, r"1/20" -> 2, r"2/1" -> 1)
	val b = Poly(r"1/4" -> 3, r"1/10" -> 2, r"3/1" -> 1)

	val aeval = a(r"2/1")
	val beval = b(r"1/2")

	println(s"\na = $a")
	println(s"b = $b\n")
	println(s"a + b = ${a + b}")
	println(s"a - b = ${a - b}")
	println(s"a * b = ${a * b}")
	println(s"gcd(a,b) = ${gcd(a,b)}")
	println(s"a % b = ${a % b}")
	println(s"a /~ b = ${a /~ b}")
	println(s"a /% b = ${a /% b}")
	println(s"a.monic = ${a.monic}")
	println(s"b.monic = ${b.monic}")
	println(s"a's derivative = ${a.derivative}")
	println(s"b's derivative = ${b.derivative}")
	println(s"a's integral ${a.integral}")
	println(s"b's integral ${b.integral}")
	println(s"a evaluated with x = 2/1 = $aeval")
	println(s"b evaluated with x = 1/2 = $beval")

}
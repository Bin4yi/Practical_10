object RationalExample {

  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be non-zero")

    private val g = gcd(x.abs, y.abs)
    val numer: Int = x / g
    val denom: Int = y / g

    def this(x: Int) = this(x, 1)

    def +(that: Rational): Rational =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
      )

    def -(that: Rational): Rational =
      new Rational(
        numer * that.denom - that.numer * denom,
        denom * that.denom
      )

    def *(that: Rational): Rational =
      new Rational(numer * that.numer, denom * that.denom)

    def /(that: Rational): Rational =
      new Rational(numer * that.denom, denom * that.numer)

    def neg: Rational = new Rational(-numer, denom)

    def sub(that: Rational): Rational = this - that

    override def toString: String = s"$numer/$denom"

    private def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  }

  def main(args: Array[String]): Unit = {
    val x = new Rational(3, 4)
    val y = new Rational(5, 8)
    val z = new Rational(2, 7)

    val result = x.sub(y).sub(z)

    println(s"Result of $x - $y - $z = $result")
  }
}

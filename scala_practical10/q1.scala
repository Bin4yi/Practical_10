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

    override def toString: String = s"$numer/$denom"

    private def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  }

  def main(args: Array[String]): Unit = {
    val x = new Rational(3, 4)
    println(x.neg)
  }
}

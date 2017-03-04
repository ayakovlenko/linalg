package linalg

package object implicits {

  implicit class DoubleLike(x: Double) {

    private val DOUBLE_PRECISION = 1e-10

    def ~=(y: Double, precision: Double): Boolean = {
      (x - y).abs < precision
    }

    def ~=(y: Double): Boolean = {
      x ~= (y, DOUBLE_PRECISION)
    }

    def roundedAt(p: Int): Double = {
      val s = math.pow(10, p)

      math.round(x * s) / s
    }
  }
}

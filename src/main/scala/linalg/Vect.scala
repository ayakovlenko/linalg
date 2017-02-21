package linalg

import scala.collection.immutable.IndexedSeq

case class Vect(iterables: Iterable[Double]) extends IndexedSeq[Double] {

  override val seq: IndexedSeq[Double] = iterables.toIndexedSeq

  def +(that: Vect): Vect = {
    require(this.seq.length == that.seq.length, "addition of vectors of different length")

    Vect {
      this.seq zip that.seq map {
        case (a, b) => a + b
      }
    }
  }

  def +(x: Double): Vect = Vect(seq map (_ + x))

  def -(that: Vect): Vect = {
    require(this.seq.length == that.seq.length, "subtraction of vectors of different length")

    this + (that * -1)
  }

  def -(x: Double): Vect = this + -x

  def *(scalar: Double): Vect = Vect(seq map (_ * scalar))

  def *(that: Vect): Vect = {
    require(this.seq.length == that.seq.length, "multiplication of vectors of different length")

    Vect {
      this.seq zip that.seq map {
        case (a, b) => a * b
      }
    }
  }

  def /(scalar: Double): Vect = {
    Vect(seq map (_ / scalar))
  }

  def magnitude: Double = math.sqrt((seq map (x => math.pow(x, 2))).sum)

  def unit: Vect = this * (1 / magnitude)

  def dot(that: Vect): Double = (this * that).seq.sum

  def angle(that: Vect): Double = math.acos {
    (this dot that) / (this.magnitude * that.magnitude)
  }

  def isZero: Boolean = this.seq forall (_ == 0)

  def isParallelTo(that: Vect): Boolean = {
    if (this.isZero || that.isZero) true else (this angle that) ~= (math.Pi, precision)
  }

  def isOrthogonalTo(that: Vect): Boolean = {
    if (this.isZero || that.isZero) true else (this dot that) ~= (0, precision)
  }

  /**
    * Projects this vector onto base vector.
    *
    * @param base base vector
    * @return a tuple of components parallel and orthonogal to base vector
    */
  def projectOnto(base: Vect): (Vect, Vect) = {
    val parallel = base.unit * (this dot base.unit)
    val orthogonal = this - parallel

    (parallel, orthogonal)
  }

  def cross(that: Vect): Vect = {
    require(this.length == 3 && that.length == 3)

    val (x1, y1, z1) = (this(0), this(1), this(2))
    val (x2, y2, z2) = (that(0), that(1), that(2))

    Vect.of(
      y1 * z2 - y2 * z1,
      -(x1 * z2 - x2 * z1),
      x1 * y2 - x2 * y1
    )
  }

  override def toString: String = seq.mkString("<[", ", ", "]>")

  private val precision = 1e-12

  private implicit class DoubleLike(x: Double) {

    def ~=(y: Double, precision: Double): Boolean = {
      (x - y).abs < precision
    }
  }

  override def length: Int = seq.length

  override def iterator: Iterator[Double] = seq.iterator

  override def apply(idx: Int): Double = seq(idx)
}

object Vect {

  def of(seq: Double*): Vect = Vect(seq)

  def fill(len: Int)(elem: Double): Vect = Vect(1 to len map (_ => elem))
}

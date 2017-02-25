package linalg

import linalg.implicits._

import scala.collection.immutable.IndexedSeq

case class Vect(iterables: Iterable[Double]) extends IndexedSeq[Double] {

  override val seq: IndexedSeq[Double] = iterables.toIndexedSeq

  require(seq.nonEmpty, "vector cannot be empty")

  def +(that: Vect): Vect = {
    require(this.length == that.length, "addition of vectors of different length")

    Vect {
      this zip that map {
        case (a, b) => a + b
      }
    }
  }

  def +(x: Double): Vect = this + (Vect fill this.size)(x)

  def -(that: Vect): Vect = {
    require(this.length == that.length, "subtraction of vectors of different length")

    this + (that * -1)
  }

  def -(x: Double): Vect = this + -x

  def *(scalar: Double): Vect = this * (Vect fill this.length)(scalar)

  def *(that: Vect): Vect = {
    require(this.length == that.length, "multiplication of vectors of different length")

    Vect {
      this zip that map {
        case (a, b) => a * b
      }
    }
  }

  def /(scalar: Double): Vect = {
    Vect(seq map (_ / scalar))
  }

  def magnitude: Double = math.sqrt(this dot this)

  def unit: Vect = this * (1 / magnitude)

  def dot(that: Vect): Double = (this * that).sum

  def angle(that: Vect): Double = math.acos {
    (this dot that) / (this.magnitude * that.magnitude)
  }

  def isUnit: Boolean = this.magnitude == 1

  def isZero: Boolean = this forall (_ == 0)

  def isParallelTo(that: Vect): Boolean = {
    if (this.isZero || that.isZero) true else (this angle that) ~= math.Pi
  }

  def isOrthogonalTo(that: Vect): Boolean = {
    if (this.isZero || that.isZero) true else (this dot that) ~= 0
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

  override def toString: String = {
    val p = 6
    seq.map(_ roundedAt p).mkString("[ ", ", ", " ]")
  }

  override def length: Int = seq.length

  override def iterator: Iterator[Double] = seq.iterator

  override def apply(idx: Int): Double = seq(idx)
}

object Vect {

  def of(seq: Double*): Vect = Vect(seq)

  def of(range: Range): Vect = Vect(range map (_.toDouble))

  def fill(len: Int)(elem: Double): Vect = Vect(1 to len map (_ => elem))
}

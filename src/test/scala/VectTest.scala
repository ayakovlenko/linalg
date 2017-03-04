import linalg.Vect
import org.scalatest.FunSuite

class VectTest extends FunSuite {

  test("misc") {

    assert(Vect(8.0, 12.0, -1.0, 0.0) === Vect(2, 4, -1, 0) * 3 + Vect(1, 0, 1, 0) * 2)
  }
}

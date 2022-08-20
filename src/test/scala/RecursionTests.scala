import org.scalatest.funsuite.AnyFunSuite
import Recursion._

class RecursionTests extends AnyFunSuite {

  test("fib(6) is 5") {
    assertResult(5)(fib(6))
  }

  test("fib(0) is 0") {
    assertResult(0)(fib(0))
  }

  def compareInts(a: Int, b: Int) = a <= b

  test("isSorted straightforward with Ints") {
    assert(isSorted(Array(1, 2, 3, 4), compareInts))
  }

  test("isSorted with repeat Ints") {
    assert(isSorted(Array(1, 2, 3, 3, 4), compareInts))
  }

  test("isSorted with unsorted Int Array") {
    assert(!isSorted(Array(1, 2, 1, 3), compareInts))
  }

  def compareStringsByLength(a: String, b: String) = a.length <= b.length

  test("isSorted with sorted string lengths") {
    assert(isSorted(Array("I", "am", "not", "false"), compareStringsByLength))
  }

  test("isSorted with unsorted string array") {
    assert(!isSorted(Array("I", "am", "definitely", "false"), compareStringsByLength))
  }
}

import org.scalatest.funsuite.AnyFunSuite
import com.rspears.Tree._
import com.rspears._

class TreeTests extends AnyFunSuite {
  test("size test") {
    assertResult(5)(size(Branch(Leaf(1), Branch(Leaf(12), Leaf(4)))))
  }

  test("maximum test with a tree") {
    assertResult(20)(maximum(Branch(Leaf(1), Branch(Leaf(20), Branch(Leaf(1), Leaf(3))))))
  }

  test("maximum test with a single leaf") {
    assertResult(1)(maximum(Leaf(1)))
  }

  test("depth of a Tree") {
    assertResult(5)(depth(Branch(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))), Leaf(0))))
  }

  test("depth of a single Leaf") {
    assertResult(1)(depth(Leaf(0)))
  }

  test("map - multiply everything by two") {
    assertResult(Branch(Leaf(2), Branch(Leaf(4), Leaf(6))))(map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ * 2))
  }
}

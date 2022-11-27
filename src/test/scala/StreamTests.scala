import org.mockito.MockitoSugar
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import com.rspears.laziness.Stream

class StreamTests extends AnyFunSuite with Matchers with MockitoSugar {
  test("toList") {
    Stream(1, 2, 3+4).toList shouldBe List(1, 2, 7)
  }

  test("take") {
    Stream(1, 2, 3, 4).take(2).toList shouldBe List(1, 2)
  }

  test("drop") {
    Stream(1, 2, 3, 4).drop(2).toList shouldBe List(3, 4)
  }

  test("takeWhile") {
    Stream(1, 2, 3, 4).takeWhile(_ <= 3).toList shouldBe List(1, 2, 3)
  }

  test("forAll true") {
    Stream(1, 2, 3).forAll(_ > 0) shouldBe true
  }

  test("forAll false") {
    Stream(1, 2, 3).forAll(_ < 3) shouldBe false
  }

  test("forAll2 true") {
    Stream(1, 2, 3).forAll2(_ > 0) shouldBe true
  }

  test("forAll2 false") {
    Stream(1, 2, 3).forAll2(_ < 3) shouldBe false
  }

  test("map") {
    Stream(1, 2, 3).map(_ * 2).toList shouldBe List(2, 4, 6)
  }

  test("filter") {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList shouldBe List(2, 4)
  }

  test("append") {
    Stream(1, 2).append(Stream(3, 4)).toList shouldBe List(1, 2, 3, 4)
  }

  test("flatMap") {
    Stream(1, 2).flatMap(x => Stream(x, x)).toList shouldBe List(1, 1, 2, 2)
  }

  test("ones 1") {
    Stream.ones.take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  test("ones 2") {
    Stream.ones.map(_ + 1).take(3).toList shouldBe List(2, 2, 2)
  }

  test("ones forAll false") {
    Stream.ones.forAll(_ != 1) shouldBe false
  }

  test("constant") {
    Stream.constant(4).take(4).toList shouldBe List(4, 4, 4, 4)
  }

  test("from") {
    Stream.from(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  test("fibs") {
    Stream.fibs.take(10).toList shouldBe {
      List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
  }

  test("onesUnfold") {
    Stream.onesUnfold.take(3).toList shouldBe List(1, 1, 1)
  }

  test("constantUnfold") {
    Stream.constantUnfold(3.0).take(4).toList shouldBe List(3.0, 3.0, 3.0, 3.0)
  }

  test("fromUnfold") {
    Stream.fromUnfold(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  test("fibsUnfold") {
    Stream.fibsUnfold.take(6).toList shouldBe {
      List(0, 1, 1, 2, 3, 5)
    }
  }

  test("map via unfold") {
    Stream.map(Stream(1, 2, 3))(_ + 1).toList shouldBe List(2, 3, 4)
  }

  test("take via unfold") {
    Stream.take(Stream(1, 2, 3, 4, 5), 3).toList shouldBe List(1, 2, 3)
  }
}

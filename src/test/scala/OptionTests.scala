import org.scalatest.funsuite.AnyFunSuite
import Option._
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class OptionTests extends AnyFunSuite with Matchers {
  test("variance") {
    assertResult(Some(0.0))(variance(Seq(1, 1, 1, 1)))
  }

  test("map2") {
    assertResult(Some(5))(map2(Some(2), Some(3))(_ + _))
  }

  test("sequence") {
    sequence(MyList(Some(1), Some(2), Some(3))) shouldBe Some(MyList(1, 2, 3))
  }

  def div(num: Double, den: Double): Option[Double] = den match {
    case 0 => None
    case _ => Some(num / den)
  }

  test("traverse") {
    traverse(MyList(4, 2, 1))(div(1, _)) shouldBe Some(MyList(0.25, 0.5, 1.0))
  }

  test("traverse2") {
    traverse2(MyList(4, 2, 1))(div(1, _)) shouldBe Some(MyList(0.25, 0.5, 1.0))
  }

  test("sequence2") {
    sequence2(MyList(Some(1), Some(2), Some(3))) shouldBe Some(MyList(1, 2, 3))
  }
}

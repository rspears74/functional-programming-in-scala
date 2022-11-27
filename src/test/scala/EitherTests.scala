import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import com.rspears._
import com.rspears.Either._
import com.rspears.lists._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class EitherTests extends AnyFunSuite with Matchers {
  def div(num: Double, den: Double): Either[String, Double] = den match {
    case 0 => Left("divide by 0")
    case _ => Right(num / den)
  }

  test("traverse div by 0") {
    traverse(MyList(0, 2, 3))(div(1,_)) shouldBe Left("divide by 0")
  }

  test("traverse Right") {
    traverse(MyList(1, 2, 4))(div(1, _)) shouldBe {
      Right(Cons(1, Cons(0.5, Cons(0.25, Nil))))
    }
  }

  test("sequence normal behavior") {
    sequence(MyList(Right(1), Right(2), Right(3))) shouldBe {
      Right(MyList(1, 2, 3))
    }
  }

  test("sequence contains a Left") {
    sequence(MyList(Right(1), Left("nothing"), Right(3))) shouldBe Left("nothing")
  }

  test("sequence multiple Lefts") {
    sequence(MyList(Left("first"), Left("second"), Right(3))) shouldBe Left("first")
  }
}

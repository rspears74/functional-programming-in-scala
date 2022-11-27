import org.scalatest.funsuite.AnyFunSuite
import Laziness._
import org.mockito.scalatest.MockitoSugar
import org.scalatest.matchers.should.Matchers

class LazinessTests extends AnyFunSuite with Matchers with MockitoSugar {
  test("if2") {
    if2(false, sys.error("fail"), 3 + 2) shouldBe 5
  }

  test("lazy arg evaluated twice") {
    def addShit = 41 + 1
    def printShit = println("hi")
    def doShit = {
      printShit
      addShit
    }
    maybeTwice(true, doShit) shouldBe 84
  }

  test("lazy arg eval'd twice 2") {
    def addShit = 41 + 1
    def printShit = println("hi")
    def doShit = {
      printShit
      addShit
    }

    maybeTwice2(true, doShit) shouldBe 84
  }
}

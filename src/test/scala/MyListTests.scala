import org.scalatest.funsuite.AnyFunSuite
import MyList._

class MyListTests extends AnyFunSuite {
  test("ex 3.1") {
    assertResult(3)(ex3_1)
  }

  test("tail of regular list") {
    assertResult(tail(MyList(1, 2, 3)))(MyList(2, 3))
  }

  test("tail of 2 element list") {
    assertResult(tail(MyList(1, 2)))(MyList(2))
  }

  test("tail of Nil") {
    assertResult(tail(Nil))(Nil)
  }

  test("setHead with Ints") {
    assertResult(setHead(MyList(1, 2, 3), 4))(MyList(4, 2, 3))
  }

  test("setHead with Strings") {
    assertResult(setHead(MyList("hello"), "goodbye"))(MyList("goodbye"))
  }

  test("setHead with a Nil list") {
    assertResult(setHead(Nil, 4))(Nil)
  }

  private val dropList = MyList(1, 2, 3)
  test("drop 0") {
    assertResult(drop(dropList, 0))(dropList)
  }

  test("drop 1") {
    assertResult(drop(dropList, 1))(MyList(2, 3))
  }

  test("drop 2") {
    assertResult(drop(dropList, 2))(MyList(3))
  }

  test("drop 3") {
    assertResult(drop(dropList, 3))(Nil)
  }

  test("drop 4") {
    assertResult(drop(dropList, 4))(Nil)
  }

  private val dropWhileList = MyList(1, 2, 3, 4, 5)
  test("dropWhile x is less than 4") {
    assertResult(dropWhile(dropWhileList, (x: Int) => x < 4))(MyList(4, 5))
  }

  test("dropWhile true") {
    assertResult(dropWhile(dropWhileList, (x: Int) => true))(Nil)
  }

  test("dropWhile false") {
    assertResult(dropWhile(dropWhileList, (x: Int) => false))(dropWhileList)
  }

  test("dropWhile x == 2") {
    assertResult(dropWhile(dropWhileList, (x: Int) => x == 2))(dropWhileList)
  }

  test("dropWhile x == 1") {
    assertResult(dropWhile(dropWhileList, (x: Int) => x == 1))(MyList(2, 3, 4, 5))
  }

  test("init list of 4 ints") {
    assertResult(init(MyList(1, 2, 3, 4)))(MyList(1, 2, 3))
  }

  test("init list of 2 ints") {
    assertResult(init(MyList(1, 2)))(MyList(1))
  }

  test("init list of one element") {
    assertResult(init(MyList(1)))(Nil)
  }

  test("init Nil") {
    assertResult(init(Nil))(Nil)
  }
}

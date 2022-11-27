import org.scalatest.funsuite.AnyFunSuite
import com.rspears.lists._
import com.rspears.lists.MyList._

class MyListTests extends AnyFunSuite {
  test("ex 3.1") {
    assertResult(3)(ex3_1)
  }

  test("tail of regular list") {
    assertResult(MyList(2, 3))(tail(MyList(1, 2, 3)))
  }

  test("tail of 2 element list") {
    assertResult(MyList(2))(tail(MyList(1, 2)))
  }

  test("tail of Nil") {
    assertResult(Nil)(tail(Nil))
  }

  test("setHead with Ints") {
    assertResult(MyList(4, 2, 3))(setHead(MyList(1, 2, 3), 4))
  }

  test("setHead with Strings") {
    assertResult(MyList("goodbye"))(setHead(MyList("hello"), "goodbye"))
  }

  test("setHead with a Nil list") {
    assertResult(Nil)(setHead(Nil, 4))
  }

  private val dropList = MyList(1, 2, 3)
  test("drop 0") {
    assertResult(dropList)(drop(dropList, 0))
  }

  test("drop 1") {
    assertResult(MyList(2, 3))(drop(dropList, 1))
  }

  test("drop 2") {
    assertResult(MyList(3))(drop(dropList, 2))
  }

  test("drop 3") {
    assertResult(Nil)(drop(dropList, 3))
  }

  test("drop 4") {
    assertResult(Nil)(drop(dropList, 4))
  }

  private val dropWhileList = MyList(1, 2, 3, 4, 5)
  test("dropWhile x is less than 4") {
    assertResult(MyList(4, 5))(dropWhile(dropWhileList, (x: Int) => x < 4))
  }

  test("dropWhile true") {
    assertResult(Nil)(dropWhile(dropWhileList, (x: Int) => true))
  }

  test("dropWhile false") {
    assertResult(dropWhileList)(dropWhile(dropWhileList, (x: Int) => false))
  }

  test("dropWhile x == 2") {
    assertResult(dropWhileList)(dropWhile(dropWhileList, (x: Int) => x == 2))
  }

  test("dropWhile x == 1") {
    assertResult(MyList(2, 3, 4, 5))(dropWhile(dropWhileList, (x: Int) => x == 1))
  }

  test("init list of 4 ints") {
    assertResult(MyList(1, 2, 3))(init(MyList(1, 2, 3, 4)))
  }

  test("init list of 2 ints") {
    assertResult(MyList(1))(init(MyList(1, 2)))
  }

  test("init list of one element") {
    assertResult(Nil)(init(MyList(1)))
  }

  test("init Nil") {
    assertResult(Nil)(init(Nil))
  }

  test("length test with 6 elements") {
    assertResult(6)(length(MyList(1, 2, 3, 4, 5, 6)))
  }

  test("length of single element list") {
    assertResult(1)(length(MyList(1)))
  }

  test("length of Nil list") {
    assertResult(0)(length(Nil))
  }

  test("foldLeft addition") {
    assertResult(10)(foldLeft(MyList(1, 2, 3, 4), 0)(_ + _))
  }

  test("foldLeft multiply") {
    assertResult(6)(foldLeft(MyList(1, 2, 3), 1)(_ * _))
  }

  test("foldLeft addition with one element") {
    assertResult(1)(foldLeft(MyList(1), 0)(_ + _))
  }

  test("foldLeft with Nil") {
    assertResult(0)(foldLeft(Nil, 0)((x: Int, y: Int) => x + y))
  }

  test("sumLeft normal list") {
    assertResult(6)(sumLeft(MyList(1, 2, 3)))
  }

  test("sumLeft Nil list") {
    assertResult(0)(sumLeft(Nil))
  }

  test("productLeft normal list") {
    assertResult(24.0)(productLeft(MyList(1.0, 2.0, 3.0, 4.0)))
  }

  test("productLeft with 0.0 element") {
    assertResult(0.0)(productLeft(MyList(4.0, 0.0)))
  }

  test("productLeft with Nil") {
    assertResult(1.0)(productLeft(Nil))
  }

  test("lengthLeft with normal list") {
    assertResult(4)(lengthLeft(MyList(1, 2, 3, 4)))
  }

  test("lengthLeft with string list") {
    assertResult(1)(lengthLeft(MyList("test")))
  }

  test("lengthLeft with Nil") {
    assertResult(0)(lengthLeft(Nil))
  }

  test("reverse normal list") {
    assertResult(MyList(3, 2, 1))(reverse(MyList(1, 2, 3)))
  }

  test("reverse single element list") {
    assertResult(MyList(1))(reverse(MyList(1)))
  }

  test("reverse with Nil") {
    assertResult(Nil)(reverse(Nil))
  }

  test("append two normal lists") {
    assertResult(MyList(1, 2, 3, 4))(append(MyList(1, 2), MyList(3, 4)))
  }

  test("append a single element list") {
    assertResult(MyList(1, 2, 3))(append(MyList(1, 2), MyList(3)))
  }

  test("append a Nil") {
    assertResult(MyList(1, 2))(append(MyList(1, 2), Nil))
  }

  test("flatten some lists") {
    assertResult(MyList(1, 2, 3, 4))(flatten(MyList(MyList(1, 2), MyList(3, 4))))
  }

  test("add1 test") {
    assertResult(MyList(2, 3, 4))(add1(MyList(1, 2, 3)))
  }

  test("doublesToStrings test") {
    assertResult(MyList("1.0", "2.0"))(doublesToStrings(MyList(1.0, 2.0)))
  }

  test("map - multiply by two") {
    assertResult(MyList(2, 4, 6))(map(MyList(1, 2, 3))(_ * 2))
  }

  test("filter evens") {
    assertResult(MyList(2, 4))(filter(MyList(1, 2, 3, 4, 5))(_ % 2 == 0))
  }

  test("flatMap duplicate test") {
    assertResult(MyList(1, 1, 2, 2))(flatMap(MyList(1, 2))(i => MyList(i, i)))
  }

  test("filterViaFlatMap evens") {
    assertResult(MyList(2, 4))(filterViaFlatMap(MyList(1, 2, 3, 4, 5))(_ % 2 == 0))
  }

  test("piecewiseAdd lists") {
    assertResult(MyList(5, 7, 9))(piecewiseAdd(MyList(1, 2, 3), MyList(4, 5, 6)))
  }

  test("zipWith multiplication") {
    assertResult(MyList(4, 10, 18))(zipWith(MyList(1, 2, 3), MyList(4, 5, 6))(_ * _))
  }

  test("hasSubsequence regular lists") {
    assert(hasSubsequence(MyList(1, 2, 3), MyList(2, 3)))
  }

  test("hasSubsequence without subsequence") {
    assert(!hasSubsequence(MyList(1, 2, 3), MyList(2, 4)))
  }

  test("hasSubsequence with a Nil") {
    assert(hasSubsequence(MyList(1, 2, 3), Nil))
  }
}

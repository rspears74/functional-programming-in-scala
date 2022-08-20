sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x + product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val ex3_1 = MyList(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + MyList.sum(t)
    case _ => 101
  }

  def tail[A](lst: MyList[A]): MyList[A] = lst match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](lst: MyList[A], head: A): MyList[A] = lst match {
    case Nil => Nil
    case Cons(x, xs) => Cons(head, xs)
  }

  def drop[A](lst: MyList[A], n: Int): MyList[A] = lst match {
    case Nil => Nil
    case l if n <= 0 => l
    case Cons(_, xs) if n == 1 => xs
    case Cons(_, xs) => drop(xs, n -1)
  }

  def dropWhile[A](lst: MyList[A], f: A => Boolean): MyList[A] = lst match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => lst
  }

  def init[A](lst: MyList[A]): MyList[A] = lst match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: MyList[A], acc: B)(f: (A, B) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc)(f))
  }

  def sum2(ns: MyList[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: MyList[Double]) =
    foldRight(ns, 1.0)(_ * _)
}
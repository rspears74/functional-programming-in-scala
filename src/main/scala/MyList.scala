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
    case Cons(_, xs) => drop(xs, n - 1)
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

  def foldRight[A, B](as: MyList[A], acc: B)(f: (A, B) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc)(f))
  }

  def sum2(ns: MyList[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: MyList[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: MyList[A]): Int =
    foldRight(as, 0)((x, acc) => 1 + acc)

  def foldLeft[A, B](as: MyList[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x))(f)
  }

  def sumLeft(lst: MyList[Int]) =
    foldLeft(lst, 0)(_ + _)

  def productLeft(lst: MyList[Double]) =
    foldLeft(lst, 1.0)(_ * _)

  def lengthLeft[A](lst: MyList[A]) =
    foldLeft(lst, 0)((x, y) => x + 1)

  def reverse[A](lst: MyList[A]): MyList[A] =
    foldLeft(lst, MyList[A]())((acc, head) => Cons(head, acc))

  // couldn't figure this one out, had to look up answer
  def foldLeftWithRight[A,B](lst: MyList[A], acc: B)(f: (B, A) => B): B =
    foldRight(lst, (b: B) => b)((a, g) => b => g(f(b, a)))(acc)

  //couldn't figure this one out, had to look up answer
  def foldRightWithLeft[A,B](lst: MyList[A], acc: B)(f: (A, B) => B): B =
    foldLeft(lst, (b: B) => b)((g, a) => b => g(f(a, b)))(acc)

  def append[A](lst: MyList[A], lst2: MyList[A]): MyList[A] =
    foldRight(lst, lst2)(Cons(_, _))

  def flatten[A](lists: MyList[MyList[A]]): MyList[A] =
    foldLeft(lists, MyList[A]())(append)

  def add1(lst: MyList[Int]): MyList[Int] = lst match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, add1(xs))
  }

  def doublesToStrings(lst: MyList[Double]): MyList[String] = lst match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doublesToStrings(xs))
  }

  def map[A,B](as: MyList[A])(f: A => B): MyList[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) => filter(xs)(f)
  }

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def filterViaFlatMap[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as)(x => {
      if (f(x))
        Cons(x, Nil)
      else
        Nil
    })

  def piecewiseAdd(lst1: MyList[Int], lst2: MyList[Int]): MyList[Int] = (lst1, lst2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, piecewiseAdd(t1, t2))
  }

  def zipWith[A,B,C](l1: MyList[A], l2: MyList[B])(f: (A, B) => C): MyList[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  //could not figure out, looked up answer
  @annotation.tailrec
  def hasSubsequence[A](lst: MyList[A], sub: MyList[A]): Boolean = {
    @annotation.tailrec
    def startsWith[A](lst: MyList[A], prefix: MyList[A]): Boolean = (lst, prefix) match {
      case (_, Nil) => true //I don't understand this case
      case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
      case _ => false
    }

    lst match {
      case Nil => sub == Nil
      case _ if startsWith(lst, sub) => true
      case Cons(h, t) => hasSubsequence(t, sub)
    }
  }
}
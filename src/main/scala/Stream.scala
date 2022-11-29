package com.rspears.laziness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, _) if n == 0 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n == 0 => Cons(h, () => t().drop(0))
    case Cons(h, t) => t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }
  def forAll2(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => {
      if (p(a))
        Stream.cons(a, b)
      else
        Stream.empty
    })

  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => {
      if (f(a))
        Stream.cons(a, b)
      else
        b
    })

  def append[B>:A](a: => Stream[B]): Stream[B] =
    foldRight(a)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fib(n: Int, m: Int): Stream[Int] =
      Stream.cons(n, fib(m, n+m))
    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case _ => Stream.empty
  }

  def onesUnfold = unfold(1)(x => Some(1, 1))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1))

  def fibsUnfold: Stream[Int] = unfold((0, 1)) {
    case (f0, f1) => Some(f0, (f1, f0 + f1))
  }

  def map[A, B](as: Stream[A])(f: A => B): Stream[B] =
    unfold(as) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def take[A](as: Stream[A], n: Int): Stream[A] =
    unfold((as, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhile[A](as: Stream[A])(p: A => Boolean): Stream[A] =
    unfold(as) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[A,B,C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((as, bs)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[A,B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((as, bs)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  def startsWith[A](s: Stream[A], s1: Stream[A]): Boolean = {
    zipAll(s, s1).takeWhile(!_._2.isEmpty).forAll {
      case (a, b) => a == b
    }
  }

  def tails[A](s: Stream[A]): Stream[Stream[A]] = {
    unfold(s) {
      case Cons(h, t) => Some((Cons(h, t), t()))
      case _ => None
    }
  }

  def hasSubsequence[A](s: Stream[A], s1: Stream[A]): Boolean =
    tails(s).exists(startsWith(_, s1))

  def scanRight[A, B](s: Stream[A])(z: B)(f: (A, => B) => B): Stream[B] = {
    s.foldRight((z, Stream(z)))((v, acc) => {
      val acc1 = acc
      val a = f(v, acc1._1)
      (a, Stream.cons(a, acc1._2))
    })._2
  }
}
package com.rspears

import com.rspears.lists._

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aVal => b.map(bVal => f (aVal, bVal)))

  def sequence[A](a: MyList[Option[A]]): Option[MyList[A]] = a match {
    case Nil => Some(Nil)
    case Cons(x, xs) => x.flatMap(xVal => sequence(xs).map(Cons(xVal, _)))
  }

  def traverse[A,B](a: MyList[A])(f: A => Option[B]): Option[MyList[B]] = a match {
    case Nil => Some(Nil)
    case Cons(x, xs) => map2(f(x), traverse(xs)(f))(Cons(_, _))
  }

  def traverse2[A,B](a: MyList[A])(f: A => Option[B]): Option[MyList[B]] =
    MyList.foldRight[A, Option[MyList[B]]](a, Some(Nil))((h, t) => map2(f(h), t)(Cons(_, _)))

  def sequence2[A](a: MyList[Option[A]]): Option[MyList[A]] =
    traverse(a)(v => v)
}
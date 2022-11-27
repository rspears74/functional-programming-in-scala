package com.rspears

import com.rspears.lists._

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(a) => Left(a)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(a) => Left(a)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list")
    else
      Right(xs.sum / xs.length)

  def saveDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {case e: Exception => Left(e)}

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e)}

  def traverse[E, A, B](lst: MyList[A])(f: A => Either[E, B]): Either[E, MyList[B]] = lst match {
    case Nil => Right(Nil)
    case Cons(x, xs) => f(x).map2(traverse(xs)(f))(Cons(_, _))
  }

  def sequence[E, A](lst: MyList[Either[E, A]]): Either[E, MyList[A]] =
    traverse(lst)(x => x)
}
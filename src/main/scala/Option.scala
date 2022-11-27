trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): MyOption[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object Option {
  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
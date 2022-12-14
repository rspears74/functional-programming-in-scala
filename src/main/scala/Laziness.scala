object Laziness {
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0

  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i // cache i - the first time j is called it will be evaluated
    if (b) j+j else 0
  }
}

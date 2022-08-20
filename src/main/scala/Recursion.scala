object Recursion {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def local(prev1: Int, prev2: Int, nCurr: Int): Int = {
      if (nCurr == n)
        prev1 + prev2
      else
        local(prev2, prev2 + prev1, nCurr + 1)
    }

    if (n <= 1) 0
    else if (n == 2) 1
    else local(0, 1, 3)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n-1), as(n))) false
      else loop(n + 1)
    }

    loop(1)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a: A => {b: B => f(a, b)}
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }
}

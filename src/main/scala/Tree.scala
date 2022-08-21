sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l).max(depth(r)))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  //struggled with all of these
  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x => 1)((a, b) => 1 + a + b)

  def maxViaFold(tree: Tree[Int]): Int =
    fold(tree)(x => x)((l, r) => l.max(r))

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x => 1)((l, r) => 1 + l.max(r))

  def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))
}
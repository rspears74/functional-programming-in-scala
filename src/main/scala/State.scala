package com.rspears.state

object StateStuff {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (r, newRng) = rng.nextInt
    r match {
      case x if x < 0 => (-(x + 1), newRng)
      case x => (x, newRng)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (List.empty, rng)
    } else {
      val (i, r) = rng.nextInt
      val (is, r2) = ints(count - 1)(r)
      (i :: is, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, r) = s(rng)
      (f(a), r)
    }

  def double2: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r) = ra(rng)
      val (b, r2) = rb(r)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double2)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double2, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => {
      map2(f, acc)(_ :: _)
    })

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapFromFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2FromFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapFromFlatMap(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A,B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S,A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](List()))((v, acc) => {
      v.map2(acc)(_ :: _)
    })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (m: Machine) => {
    (i, m) match {
      case (_, Machine(_, 0, _)) => m
      case (Coin, Machine(false, _, _)) => m
      case (Turn, Machine(true, _, _)) => m
      case (Coin, Machine(true, ca, co)) => Machine(false, ca, co + 1)
      case (Turn, Machine(false, ca, co)) => Machine(true, ca - 1, co)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(i => State.modify[Machine](update(i))))
    s <- State.get
  } yield (s.coins, s.candies)
}

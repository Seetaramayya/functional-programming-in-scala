package com.fpinscala.chapter6.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    if (nextInt < 0) (-nextInt - 1, nextRng) else (nextInt, nextRng)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRng) = nonNegativeInt(rng)
    (nextInt.toDouble / Int.MaxValue, nextRng)
  }

  def double2: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    ((nextInt, double(rng)._1), nextRng)
  }

  def intDoubleViaCombinator: Rand[(Int, Double)] = both(int, double)

  def doubleIngViaCombinator: Rand[(Double, Int)] = both(double, int)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    ((double(rng)._1, nextInt), nextRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val list = (1 to count).foldLeft(List(rng.nextInt)) {
      case ((nextInt, nextRng) :: tail, _) => nextRng.nextInt :: (nextInt, nextRng) :: tail
    }

    (list.map(_._1), list.last._2)
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = {
    sequence2(List.fill(count)(int))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    val list = fs.foldRight(List[(A, RNG)]()) {
      case (randA, acc) => randA(rng) :: acc
    }
    (list.map(_._1), list.last._2)
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((randA, acc) => map2(randA, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(a => if (a + (n - 1) - (a %n) >=0 ) unit(a % n) else nonNegativeLessThan(n))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s1) = run(s)
    (f(a), s1)
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)
    val (b, s2) = f(a).run(s1)
    (b, s2)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def isEmpty: Boolean = candies <= 0
  def nonEmpty: Boolean = !isEmpty
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] = ls.foldRight(unit[S, List[A]](List())){
    case (state, acc) => state.map2(acc)(_ :: _)
  }

  def modify[S](f: S => S): State[S, Unit] = State { s => ((), f(s)) }

  def update = (i: Input) => (m: Machine) => (i, m) match {
    case (Coin, Machine(true, candies, coins)) if candies > 0 => m.copy(locked = false, coins = coins + 1)
    case (Turn, Machine(false, candies, _))                   => m.copy(locked = true, candies = candies - 1)
    case _                                                    => m
  }

  /**
    * 1. Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    * 2. Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    * 3. Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    * 4. A machine that’s out of candy ignores all inputs.
    */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val temp = inputs.map(input => modify[Machine](machine => update(input)(machine)))
    val state: State[Machine, Machine] = get[Machine]

    state.flatMap { machine =>
      sequence(temp).map { _ =>
        (machine.coins, machine.candies)
      }
    }
  }

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val temp = inputs.map(input => modify[Machine](machine => update(input)(machine)))

    for {
      machine <- get[Machine]
      _ <- sequence(temp)
    } yield (machine.coins, machine.candies)
  }
}

package com.fpinscala.chapter5.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /**
    * Exercise 5.1: convert to List
    * @return list of this stream
    */
  def toList: List[A] = foldRight(List[A]())((a, b) => a :: b)

  /**
    * Exercise 5.2 (1): returns first `n` elements of the stream
    * @param n number of elements
    * @return first n elements of the stream
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /**
    * Exercise 5.2 (2): skip first `n` elements of the stream and return rest of the stream
    * @param n number of elements
    * @return stream without first n elements of the stream
    */
  def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case _ if n == 0 => this
    case Cons(_, t) if n >= 1 => t().drop(n - 1)
  }

  /**
    * Exercise 5.3: Write a function for returning all starting elements of a
    * Stream that match the given predicate
    *
    * @param p predicate (is a function that takes element and returns boolean value)
    * @return all starting elments that match given predicate
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def dropWhile(p: A => Boolean): Stream[A] = this match {
    case Empty                => empty
    case Cons(h, t) if p(h()) => t().dropWhile(p)
    case Cons(h, t)           => Cons(h, t)
  }

  /**
    * Exercise 5.4: Implement forAll using foldRight
    * @param p predicate (is a function that takes element and returns boolean value)
    * @return true if it is true for all elements otherwise false
    */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
    * Exercise 5.5: Implement takeWhile using foldRight
    * @param p predicate (is a function that takes element and returns boolean value)
    * @return all starting elments that match given predicate
    */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty )

  /**
    * Exercise 5.6: Implement headOption using foldRight
    * @return first element wrapped in Some otherwise None
    */
  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  /**
    * Exercise 5.7 (1): implement map using foldRight
    */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  /**
    * Exercise 5.7 (2): implement map using foldRight
    */
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if(p(h)) cons(h, t) else t)

  // This will not execute rest of the stream
  def findViaFilter(p: A => Boolean): Option[A] = filter(p).headOption

  /**
    * Exercise 5.7 (3): implement append using foldRight
    */
  def append[B >: A](s2: => Stream[B]): Stream[B] = foldRight(s2)((h, t) => cons(h, t))

  /**
    * Exercise 5.7 (4): implement append using foldRight
    */
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  /**
    * Exercise 5.13: implement map, take, takeWhile, zipWith and zipAll using unfold
    */
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }
  def takeViaUnfold(n: Int): Stream[A] = unfold(this) {
    case Cons(h, t) if n >= 1 => Some((h(), t().takeViaUnfold(n - 1)))
    case Cons(h, _) if n == 1 => Some((h(), empty))
    case _                    => None
  }
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some(h(), t().takeWhileViaUnfold(f))
    case _                    => None
  }
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Empty, _)                   => None
    case (_, Empty)                   => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Empty, Empty)               => None
    case (Empty, Cons(h2, t2))        => Some((None, Some(h2())), (Empty, t2()))
    case (Cons(h1, t1), Empty)        => Some((Some(h1()), None), (t1(), Empty))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  /**
    * Exercise 5._
    */
  def hasSubSequenceFirstWay[A1 >: A](sub: Stream[A1]): Boolean = dropWhile(a => !sub.headOption.contains(a)).zipWith(sub)(_ == _).forAll(identity)

  def hasSubSequence[B >: A](s: Stream[B]): Boolean = tails.exists(_.startsWith(s))

  /**
    * Exercise 5.14: Implement start with using functions above, it checks one stream is a prefix of another
    */
  def startsWith[B](s: Stream[B]): Boolean = this match {
    case Empty if s != Empty => false
    case _                   => zipWith(s)(_ == _).forAll(identity)
  }

  /**
    * Exercise 5.15: Implement using unfold
    */
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case stream => Some((stream, stream.drop(1)))
  } append Stream(empty)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = tails.map(s => s.foldRight(z)(f))

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

  def single[A](a: A): Stream[A] = cons(a, Empty)

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 5.8: Creates infinite stream of given element
    */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  val ones: Stream[Int] = Stream.constant(1)

  /**
    * Exercise 5.9: generate infinite stream of integers from given number
    */
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // Natural numbers
  val naturalNumbers: Stream[Int] = from(0)

  /**
    * Exercise 5.10: Creates infinite stream of given element
    */
  def fibs(): Stream[Int] = {
    def loop(first: Int, second: Int, acc: Stream[Int]): Stream[Int] = {
      acc.append(cons(first + second, loop(second, first + second, empty)))
    }

    loop(0, 1, Stream(0, 1))
  }

  /**
    * Exercise 5.11: Write a function that takes initial state and function that produces
    * next state and next value in the generated stream
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map {
      case (a, s) => cons(a, unfold(s)(f))
    }.getOrElse(empty)
  }

  /**
    * Exercise 5.12: fibs, from, constant and ones in terms of unfold
    */
  val onesViaUnfold: Stream[Int] = unfold(1)(n => Some((n, 1)))
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(n => Some((n, n)))
  def fibsViaUnfold(): Stream[Int] = unfold((0, 1): (Int, Int)) {
    case (n1, n2) => Some((n1, (n2, n1 + n2)))
  }
}

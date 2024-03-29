package com.fpinscala.chapter3.datastructures

import scala.annotation.tailrec


// `List` data type, parameterized on a type, `A`
sealed trait List[+A]

// A `List` data constructor representing the empty list
case object Nil extends List[Nothing]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =  foldRight(ns, 1.0)(_ * _)
  def product3(ns: List[Double]): Double =  foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil           => throw new UnsupportedOperationException("tail of empty list is not allowed")
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = if (n <= 0 || l == Nil) l else drop(tail(l), n - 1)

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, tail) if f(h) => Cons(h, tail)
    case Cons(_, tail) => dropWhile(tail)(f)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, tail) => Cons(h, init(tail))
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, tail) => length(tail) + 1
  }

  def length2[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)
  def length3[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, elem) => Cons(elem, acc))

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (identity: B) => identity)((a, g) => b => g(f(b, a)))(z)
  }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (identity: B) => identity)((g, a) => b => g(f(a, b)))(z)
  }

  def append2[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((elem, acc) => Cons(elem, acc))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append2)

  /**
    * Exercise 3.16: map function can be used but implementing differently
    * @param l list of integers
    * @return list of integers with plus one
    */
  def map2(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, tail) => Cons(h + 1, map2(tail))
  }

  /**
    * Exercise 3.17: map function can be used but implementing differently
    * @param l list of integers
    * @return list of integers with plus one
    */
  def map3(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(h, tail) => Cons(h.toString, map3(tail))
  }

  // Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, List[A]())((elem, acc) => if (f(elem)) Cons(elem, acc) else acc)

  // Exercise 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, List[B]())((elem, acc) => append(f(elem), acc))

  // Exercise 3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if(f(a)) List(a) else List())

  def zip[A, B](l1: List[A], l2: List[B]): List[(A, B)] = zipWith(l1, l2)((a, b) => (a, b))

  // Exercise 3.23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, xs1), Cons(h2, xs2)) => Cons(f(h1, h2), zipWith(xs1, xs2)(f))
  }

  // Exercise 3.22
  def merge(l1: List[Int], l2: List[Int]): List[Int] = map(zip[Int, Int](l1, l2)) {
    case (a, b) => a + b
  }

  // Exercise 3.22.1
  def merge2(l1: List[Int], l2: List[Int]): List[Int] = zipWith(l1, l2)(_ + _)

  // Exercise 3.24
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => foldLeft(zipWith(t1, t2)(_ == _), true)(_ && _)
    case (Cons(_, t1), Cons(h2, t2)) => hasSubSequence(t1, Cons(h2, t2))
  }
}

package com.fpinscala.chapter3.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  // Exercise 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // Exercise 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => (depth(left) + 1) max (depth(right) + 1)
  }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + l + r)

  def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(identity)((l, r) => l max r)

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((l, r) => 1 + (l max r))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    val f1: A => Tree[B] = a =>  Leaf(f(a))
    fold(tree)(f1)(Branch(_, _))
  }
}

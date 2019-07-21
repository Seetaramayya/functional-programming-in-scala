package com.fpinscala.chapter3.datastructures

import org.scalatest.{Matchers, WordSpec}
import List._

class ListSpec extends WordSpec with Matchers {

  "List" should {
    "respond with 'UnsupportedOperationException' when given list is empty for: '#tail'" in {
      intercept[UnsupportedOperationException] {
        tail(Nil)
      }
    }

    "return tail for non empty list for: '#tail'" in {
      tail(List(1)) shouldBe Nil
      tail(List(1, 2)) shouldBe List(2)
      tail(List(1, 2, 3)) shouldBe List(2, 3)
    }

    "return new list with given head element for: '#setHead'" in {
      setHead(Nil, 4) shouldBe List(4)
      setHead(List(1, 2, 3), 4) shouldBe List(4, 2, 3)
    }

    "return remaining elements after removing given number of elements for: '#drop'" in {
      drop(Nil, 4) shouldBe Nil
      drop(List(1, 2, 3), 1) shouldBe List(2, 3)
      drop(List(1, 2, 3), 2) shouldBe List(3)
      drop(List(1, 2, 3), 3) shouldBe Nil
    }

    "drop the elements while the given condition is met for: '#dropWhile'" in {
      dropWhile(List(1, 2, 3))(_ => false) shouldBe Nil
      dropWhile(List(1, 2, 3))(_ => true) shouldBe List(1, 2, 3)
      dropWhile(List(1, 2, 3))( _ == 3) shouldBe List(3)
      dropWhile(Nil)( _ => true) shouldBe Nil
    }

    "return all elements except last element for: '#init'" in {
      init(List(1, 2, 3)) shouldBe List(1, 2)
      init(List(1, 2)) shouldBe List(1)
      init(List(1)) shouldBe Nil
      init(Nil) shouldBe Nil
    }

    "return number of elements in the list for: '#length'" in {
      List.length(List(1, 2, 3)) shouldBe 3
      List.length2(List(1, 2, 3)) shouldBe 3
      List.length3(List(1, 2, 3)) shouldBe 3

      List.length(List(1, 2)) shouldBe 2
      List.length2(List(1, 2)) shouldBe 2
      List.length3(List(1, 2)) shouldBe 2
    }

    "return number of elements in the list for: '#foldLeft'" in {
      foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
      foldLeft(List(1, 2, 3), 0)(_ * _) shouldBe 0
      foldLeft(List(1, 2, 3), 1)(_ * _) shouldBe 6
      foldLeft(List(1, 2, 3), List[Int]())((acc, elem) => Cons(elem, acc)) shouldBe List(3, 2, 1)
    }

    "return number of elements in the list for: '#foldRight'" in {
      foldRight(List(1, 2, 3), 0)(_ + _) shouldBe 6
      foldRight(List(1, 2, 3), 0)(_ * _) shouldBe 0
      foldRight(List(1, 2, 3), 1)(_ * _) shouldBe 6
      foldRight(List(1, 2, 3), List[Int]())((elem, acc) => Cons(elem, acc)) shouldBe List(1, 2, 3)
    }

    "return number of elements in the list for: '#foldLeftViaFoldRight'" in {
      foldLeftViaFoldRight(List(1, 2, 3), List[Int]())((acc, elem) => Cons(elem, acc)) shouldBe List(3, 2, 1)
      foldLeftViaFoldRight(List(1, 2, 3), 0)(_ * _) shouldBe 0
      foldLeftViaFoldRight(List(1, 2, 3), 1)(_ * _) shouldBe 6
    }

    "return number of elements in the list for: '#foldRightViaFoldLeft'" in {
      foldRightViaFoldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
      foldRightViaFoldLeft(List(1, 2, 3), 0)(_ * _) shouldBe 0
      foldRightViaFoldLeft(List(1, 2, 3), 1)(_ * _) shouldBe 6
      foldRightViaFoldLeft(List(1, 2, 3), List[Int]())((elem, acc) => Cons(elem, acc)) shouldBe List(1, 2, 3)
    }

    "return number of elements in the list for: '#map'" in {
      map(List(1, 2, 3))(_ + 1) shouldBe List(2, 3, 4)
      map2(List(1, 2, 3)) shouldBe List(2, 3, 4)

      map(List(1, 2, 3))(_ * 2) shouldBe List(2, 4, 6)
      map(List[Int]())(_ * 2) shouldBe Nil
    }

    "return the list after applying given function for: '#flatMap'" in {
      flatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
    }

    "return sum of elements in the list for: '#sum3'" in {
      sum3(List(1, 2, 3)) shouldBe 6
      sum3(List(1, 2)) shouldBe 3
      sum3(List(1)) shouldBe 1
      sum3(List()) shouldBe 0
    }

    "return sum of elements in the list for: '#product3'" in {
      product3(List(1, 2, 3)) shouldBe 6.0
      product3(List(1, 2)) shouldBe 2.0
      product3(List(1)) shouldBe 1.0
      product3(List()) shouldBe 1.0
    }

    "return reverse of the given list for: '#reverse'" in {
      reverse(List()) shouldBe List()
      reverse(List(1)) shouldBe List(1)
      reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    }

    "return appended list for given lists for: '#append2'" in {
      append2(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
      append2(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
      append2(Nil, List(4, 5, 6)) shouldBe List(4, 5, 6)
    }

    "return flatten list : '#concat'" in {
      concat(List(List(1, 2), List(3), List(4, 5, 6))) shouldBe List(1, 2, 3, 4, 5, 6)
    }

    "return elements that satisfy the predicate with : '#filter'" in {
      filter(List(1, 2, 3, 4, 5, 6, 7))(_ % 2 == 0) shouldBe List(2, 4, 6)
      filter2(List(1, 2, 3, 4, 5, 6, 7))(_ % 2 == 0) shouldBe List(2, 4, 6)

      filter(List(1, 2, 3, 4, 5, 6, 7))(_ % 2 != 0) shouldBe List(1, 3, 5, 7)
      filter2(List(1, 2, 3, 4, 5, 6, 7))(_ % 2 != 0) shouldBe List(1, 3, 5, 7)
    }

    "return zipped elements for given lists with : '#zip'" in {
      zip(List(1, 2, 3, 4), List(5, 6)) shouldBe List((1, 5), (2, 6))
      zip(List(1), List(5, 6, 7 ,8)) shouldBe List((1, 5))
      zip(List(), List(5, 6, 7 ,8)) shouldBe List()
      zip(List(1, 2, 3), List()) shouldBe List()

      zip(List(1, 2, 3), List(4, 5, 6)) shouldBe List((1, 4), (2, 5), (3, 6))
    }

    "return sum of two equal size integer lists with : '#merge'" in {
      merge(List(1, 2, 3, 4), List(5, 6)) shouldBe List(6, 8)
      merge2(List(1, 2, 3, 4), List(5, 6)) shouldBe List(6, 8)

      merge(List(1), List(5, 6, 7 ,8)) shouldBe List(6)
      merge2(List(1), List(5, 6, 7 ,8)) shouldBe List(6)

      merge(List(), List(5, 6, 7 ,8)) shouldBe List()
      merge2(List(), List(5, 6, 7 ,8)) shouldBe List()

      merge(List(1, 2, 3), List()) shouldBe List()
      merge2(List(1, 2, 3), List()) shouldBe List()

      merge(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
      merge2(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
    }

    "return true if given sub sequence present in sup sequence otherwise false: '#hasSubSequence'" in {
      hasSubSequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) shouldBe true
      hasSubSequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
      hasSubSequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
      hasSubSequence(List(1, 2, 3, 4), List(4)) shouldBe true
      hasSubSequence(List(1, 2, 3, 4), Nil) shouldBe true

      hasSubSequence(List(1, 2, 3, 4), List(3, 2)) shouldBe false
    }
  }
}

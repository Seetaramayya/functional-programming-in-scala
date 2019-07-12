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
  }
}

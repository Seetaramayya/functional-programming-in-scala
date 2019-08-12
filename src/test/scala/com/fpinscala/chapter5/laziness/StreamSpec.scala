package com.fpinscala.chapter5.laziness

import org.scalatest.{FunSuite, Matchers}

/**
  *
  * @author Seeta (Ramayya) Vadali
  */
class StreamSpec extends FunSuite with Matchers {

  test("testTakeWhile") {
    Stream(2, 2, 2, 4, 5).takeWhile(_ == 2).toList shouldBe List(2, 2, 2)
    Stream(2, 2, 2, 4, 5).takeWhileViaFoldRight(_ == 2).toList shouldBe List(2, 2, 2)
    Stream(2, 2, 2, 4, 5).takeWhileViaFoldRight(_ % 2 == 0).toList shouldBe List(2, 2, 2, 4)

    Stream(1, 2, 2, 4, 5).takeWhile(_ == 2).toList shouldBe List()
    Stream(1, 2, 2, 4, 5).takeWhileViaFoldRight(_ == 2).toList shouldBe List()
  }

  test("testFind") {
    Stream.naturalNumbers.find(_ == 10) shouldBe Some(10)
    Stream.naturalNumbers.findViaFilter(_ == 10) shouldBe Some(10)
  }

  test("testList") {
    Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
    Stream(1).toList shouldBe List(1)
    Stream().toList shouldBe List()
  }

  test("testTake") {
    Stream(1, 2, 3, 4, 5).take(0) shouldBe Empty
    Stream(1, 2, 3, 4, 5).take(1).toList shouldBe List(1)
    Stream(1, 2, 3, 4, 5).take(3).toList shouldBe List(1, 2, 3)
    Stream(1, 2, 3, 4, 5).take(6).toList shouldBe List(1, 2, 3, 4, 5)
  }

  test("map") {
    Stream(1, 2, 3, 4, 5).map(_ + 10).toList shouldBe (11 to 15).toList
  }

  test("filter") {
    Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList shouldBe List(2, 4)
  }

  test("map -> filter") {
    Stream(1, 2, 3, 4, 5).map(_ + 10).filter(_ % 2 == 0).toList shouldBe List(12, 14)
  }

  test("find first 100 prime numbers") {
    Stream.naturalNumbers.take(10).toList shouldBe (0 to 9).toList
    val isPrime: Int => Boolean = n => !(2 until n).exists(n % _ == 0)

    Stream.naturalNumbers.drop(2).filter(isPrime).take(1000).toList
  }

  test("fibonacci numbers") {
    Stream.fibs().take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  test("testHeadOption") {
    Stream(1, 2).headOption shouldBe Some(1)
    Stream(1).headOption shouldBe Some(1)
    Stream().headOption shouldBe None
  }

  test("testAppend") {
    Stream(1, 2).append(Stream(3, 4)).toList shouldBe (1 to 4).toList
    Stream(1, 2).append(Stream()).toList shouldBe (1 to 2).toList
    Stream().append(Stream(3, 4)).toList shouldBe (3 to 4).toList
  }

  test("testDrop") {
    Stream(1, 2, 3, 4, 5).drop(0).toList shouldBe List(1, 2, 3, 4, 5)
    Stream(1, 2, 3, 4, 5).drop(1).toList shouldBe List(2, 3, 4, 5)
    Stream(1, 2, 3, 4, 5).drop(3).toList shouldBe List(4, 5)
    Stream(1, 2, 3, 4, 5).drop(6).toList shouldBe List()
  }

  test("testForAll") {
    Stream(1, 2, 3, 4, 5).forAll(_ < 6) shouldBe true
    Stream(1, 2, 3, 4, 5).forAll(_ < 5) shouldBe false
    Stream(1, 2, 3, 4, 5).forAll(_ < 2) shouldBe false
  }

  test("testExists") {
    Stream(1, 2, 3, 4, 5).exists(_ == 3) shouldBe true
    Stream(1, 2, 3, 4, 5).forAll(_ == 6) shouldBe false
  }

  test("testFoldRight") {

  }

  test("testStartsWith") {

  }

}

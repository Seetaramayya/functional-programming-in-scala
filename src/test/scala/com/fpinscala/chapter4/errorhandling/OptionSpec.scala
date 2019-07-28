package com.fpinscala.chapter4.errorhandling

import com.fpinscala.chapter4.errorhandling.Option._
import org.scalatest.{Matchers, WordSpec}

class OptionSpec extends WordSpec with Matchers {
  private val none: Option[Int] = None

  "OptionSpec" should {

    "orElse" in {
      Some(2).orElse {
        // This should not be executed, so running unterminated operation
        var i = 0
        while(true)
          i += 0
        Some(i)
      } shouldBe Some(2)
      (none orElse Some(2)) === Some(2)
      (none orElse none) === None
    }

    "filter" in {
      Some(2).filter(_ % 2 == 0) === Some(2)
      Some(3).filter(_ % 2 == 0) === None
    }

    "flatMap" in {
      Some(2).flatMap(i => Some(i)) === Some(2)
      Some(2).flatMap(_ => None) === None
      none.flatMap(i => Some(i)) === None
    }

    "getOrElse" in {
      none.getOrElse(1) === 1
      Some(1).getOrElse(2) === 1
    }

    "map" in {
      none.map(_ + 1) === None
      Some(1).map(_ + 1) === Some(2)
    }

    "variance" in {
      variance((1 to 5).map(_.toDouble)) === Some(2D)
    }

    "sequence" in {
      sequence(List(Some(1), Some(2), Some(3))) === Some(List(1, 2, 3))
      sequence(List(Some(1), None, Some(3))) === None
      sequence(List()) === Some(List())
    }

  }
}

package com.fpinscala.chapter3.datastructures

import com.fpinscala.chapter3.datastructures.Tree._
import org.scalatest.{Matchers, WordSpec}

class TreeSpec extends WordSpec with Matchers {

  "Tree" should {
    "respond with number of elements in a given tree with function: '#size'" in {
      Tree.size(Leaf(1)) shouldBe 1
      Tree.sizeViaFold(Leaf(1)) shouldBe 1

      Tree.size(Branch(Leaf(1), Leaf(1))) shouldBe 3
      Tree.sizeViaFold(Branch(Leaf(1), Leaf(1))) shouldBe 3

      Tree.size(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))) shouldBe 5
      Tree.sizeViaFold(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))) shouldBe 5
    }

    "respond with maximum element in whole tree with function: '#maximum'" in {
      maximum(Leaf(1)) shouldBe 1
      maximumViaFold(Leaf(1)) shouldBe 1
      maximum(Branch(Leaf(2), Leaf(1))) shouldBe 2
      maximumViaFold(Branch(Leaf(2), Leaf(1))) shouldBe 2
      maximum(Branch(Leaf(1), Leaf(2))) shouldBe 2
      maximumViaFold(Branch(Leaf(1), Leaf(2))) shouldBe 2
      maximum(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))) shouldBe 5
      maximumViaFold(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))) shouldBe 5
      maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))) shouldBe 5
      maximumViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))) shouldBe 5
    }

    "respond with depth of the tree, with the function: '#depth'" in {
      depth(Leaf(1)) shouldBe 0
      depthViaFold(Leaf(1)) shouldBe 0

      depth(Branch(Leaf(2), Leaf(1))) shouldBe 1
      depthViaFold(Branch(Leaf(2), Leaf(1))) shouldBe 1

      depth(Branch(Leaf(1), Leaf(2))) shouldBe 1
      depthViaFold(Branch(Leaf(1), Leaf(2))) shouldBe 1

      depth(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))) shouldBe 2
      depthViaFold(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))) shouldBe 2

      depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))) shouldBe 3
      depthViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))) shouldBe 3
    }

    "respond with transformed elements for a given tree when '#map' is applied" in {
      map(Leaf(1))(_ + 1) shouldBe Leaf(2)
      map(Branch(Leaf(2), Leaf(1)))(_ + 1) shouldBe
          Branch(Leaf(3), Leaf(2))
      map(Branch(Leaf(1), Leaf(2)))(_ + 1) shouldBe
          Branch(Leaf(2), Leaf(3))
      map(Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))(_ + 1) shouldBe
          Branch(Branch(Leaf(4), Leaf(5)), Leaf(6))

      map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))))(_ + 1) shouldBe
          Branch(Branch(Leaf(2), Leaf(3)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))

      mapViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5))))(_ + 1) shouldBe
        Branch(Branch(Leaf(2), Leaf(3)), Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))
    }
  }
}

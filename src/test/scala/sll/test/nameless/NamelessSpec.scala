package sll.test.nameless

import sll.treeless._

class NamelessSpec extends org.scalatest.funspec.AnyFunSpec with org.scalatest.matchers.should.Matchers {

  describe("Nameless0Q") {
    it("Nameless0Q is blazed SLL") {
      validate_+(Nameless0Q.program)
    }
  }

  describe("Nameless1Q") {
    it("Nameless1Q is blazed SLL") {
      validate_+(Nameless1Q.program)
    }
  }

  describe("Nameless2Q") {
    it("Nameless2Q is blazed SLL") {
      validate_+(Nameless2Q.program)
    }
  }

  describe("Nameless2XQ") {
    it("Nameless2XQ is blazed SLL") {
      validate_+(Nameless2XQ.program)
    }
  }

  describe("Nameless2 examples") {
    import Programs.P2._
    import Nameless2XAST._

    it("flip(L(A()))") {
      val input = IGCall1("flip", Ctr1("L", Ctr0("A")))
      val expectedOutput = Ctr1("L", Ctr0("A"))
      assert(iEval(input) === expectedOutput)
    }

    it("flip(B(L(X()) , L(Y()))) => B(L(Y()) , L(X()))") {
      val input = IGCall1("flip", Ctr2("B", Ctr1("L", Ctr0("X")), Ctr1("L", Ctr0("Y"))))
      val expectedOutput = Ctr2("B", Ctr1("L", Ctr0("Y")), Ctr1("L", Ctr0("X")))
      assert(iEval(input) === expectedOutput)
    }
  }

  describe("Nameless3Q") {
    it("Nameless3Q is blazed SLL") {
      validate_+(Nameless3Q.program)
    }
  }
}

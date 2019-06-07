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

      assert(findUnOrderedFDefs(Nameless3Q.program) ===
        List()
      )

      // It means that the assertion about ordered variables is too strong.
      // Fortunately, it happens only for constructors!
      assert(findUnOrderedGDefs(Nameless3Q.program) ===
        List(
          ("eval1", "DCtr210"),
          ("eval2", "DCtr211"),
          ("eval3", "DCtr2_12"),
          ("eval3", "DCtr2_21"),
          ("eval3", "DCtr2_30"),
        )
      )
    }
  }

  describe("Nameless3VQ") {
    it("Nameless3VQ is blazed SLL") {
      validate_+(Nameless3VQ.program)

      assert(findUnOrderedFDefs(Nameless3VQ.program) ===
        List()
      )

      // It means that the assertion about ordered variables is too strong.
      // Fortunately, it happens only for constructors!
      assert(findUnOrderedGDefs(Nameless3VQ.program) ===
        List(
          ("eval1", "DCtr210"),
          ("eval2", "DCtr211"),
          ("eval3", "DCtr2_12"),
          ("eval3", "DCtr2_21"),
          ("eval3", "DCtr2_30"),
        )
      )
    }
  }
}

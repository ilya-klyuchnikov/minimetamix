package sll.test

import foetus.adt._
import foetus.parser.parseDefs
import sll.data._
import sll.translator._

class ConstraintsSpec extends org.scalatest.FunSpec with org.scalatest.Matchers {

  val prog1: Program =
    parseDefs {
      def gAdd(gX: Nat, y: Nat): Nat = gX match {
        case Z() => y
        case S(x) => S(gAdd(x, y))
      }

      def gEven(x: Nat): Bool = x match {
        case Z() => True()
        case S(x1) => gOdd(x1)
      }

      def gOdd(x: Nat): Bool = x match {
        case Z() => False()
        case S(x1) => gEven(x1)
      }
    }


  val prog2: Program =
    parseDefs {
      def test(x: Nat): Bool = x match {
        case Z() => test(x)
      }
    }

  val prog3: Program =
    parseDefs {
      def id[A](x: A): A = id(id(x))
    }

  val prog4: Program =
    parseDefs {
      def first[A](x: A, y: A): A = x
      def first1[A](x: A): A = first(x, x)
    }

  describe("checks") {

    import sll.constraints._

    it("prog1 is OK") {
      validate(prog1)
    }

    it("prog2 in not correct SLL") {
      (the [AssertionError] thrownBy validate(prog2)).getMessage should include("SLL: test")
    }

    it("prog3 is not treeless") {
      (the [AssertionError] thrownBy validate(prog3)).getMessage should include("treeless: id")
    }

    it("prog4 is not linear") {
      (the [AssertionError] thrownBy validate(prog4)).getMessage should include("linear: first1")
    }
  }

}

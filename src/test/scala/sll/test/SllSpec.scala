package sll.test

import common.adt._
import common.parser.parseDefs
import sll.data._
import sll.translator._

class SllSpec extends org.scalatest.funspec.AnyFunSpec with org.scalatest.matchers.should.Matchers {

  val prog1: Program =
    parseDefs {
      def gAdd(gX: Nat, y: Nat): Nat = gX match {
        case Z() => y
        case S(x) => S(gAdd(x, y))
      }

      def gMult(gX: Nat, y: Nat): Nat = gX match {
        case Z() => Z()
        case S(x) => gAdd(y, gMult(x, y))
      }

      def fSqr(x: Nat): Nat =
        gMult(x, x)

      def id[A](x: A): A = id(x)
    }

  describe("parsing subset of Scala into SLL") {

    it("prog1") {

      prog1 should equal {
        Program(
          List(
            GDef("gAdd", Pat("Z", List()), List("y"), Var("y")),
            GDef("gAdd", Pat("S", List("x")), List("y"), Ctr("S", List(GCall("gAdd", List(Var("x"), Var("y")))))),
            GDef("gMult", Pat("Z", List()), List("y"), Ctr("Z", List())),
            GDef("gMult", Pat("S", List("x")), List("y"), GCall("gAdd", List(Var("y"), GCall("gMult", List(Var("x"), Var("y")))))),
            FDef("fSqr", List("x"), GCall("gMult", List(Var("x"), Var("x")))),
            FDef("id", List("x"), FCall("id", List(Var("x")))),
          )
        )
      }
    }
  }
}

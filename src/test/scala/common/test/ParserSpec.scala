package common.test

import common.adt._
import common.ast._
import common.parser._

class ParserSpec extends org.scalatest.funspec.AnyFunSpec with org.scalatest.matchers.should.Matchers {

  describe("parsing subset of Scala into SLL") {

    it("id") {
      parseDefs {
        def id(x: Nat): Nat = x
      } should equal {
        List(
          Def("id", List("x"), Var("x")))
      }
    }

    it("natid") {
      parseDefs {
        def natid(x: Nat): Nat = x match {
          case Z() => Z()
          case S(pred) => S(natid(pred))
        }
      } should equal {
        List(
          Def("natid", List("x"),
            Case(Var("x") ,
              List(
                Branch(Pat("Z", List()), Ctr("Z", List())),
                Branch(Pat("S", List("pred")), Ctr("S", List(App("natid", List(Var("pred"))))))
              ))))
      }
    }


    it("add") {
      parseDefs {
        def add(x: Nat, y: Nat): Nat = x match {
          case Z() => y
          case S(x1) => S(add(x1, y))
        }
      } should equal {
        List(
          Def("add", List("x", "y"),
            Case(Var("x") ,
              List(
                Branch(Pat("Z", List()), Var("y")),
                Branch(Pat("S", List("x1")), Ctr("S", List(App("add", List(Var("x1"), Var("y"))))))
              ))))
      }
    }

    it("append") {
      parseDefs {
        def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
          case Nil() => ys
          case Cons(x1, xs1) => Cons(x1, append(xs1, ys))
        }
      } should equal {
        List(
          Def("append", List("xs", "ys"),
            Case(Var("xs") ,
              List(
                Branch(Pat("Nil", List()), Var("ys")),
                Branch(Pat("Cons", List("x1", "xs1")), Ctr("Cons", List(Var("x1"), App("append", List(Var("xs1"), Var("ys"))))))
              ))))
      }
    }
  }
}

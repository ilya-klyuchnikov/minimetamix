package foetus.test

import foetus.body._
import foetus.parser._
import org.scalatest._

class ParserSpec extends FunSpec with Matchers {

  implicit def stringToVar(s: String) = TVar(s)

  describe("parsing subset of Scala into SLL") {

    it("id") {
      parseDefs {
        sealed trait Nat
        def id(x: Nat): Nat = x
      } should equal {
        List(FDef("id", List("x"),
          TVar("x")))
      }
    }

    it("natid") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat
        def natid(x: Nat): Nat = x match {
          case Z() => Z()
          case S(pred) => S(natid(pred))
        }
      } should equal {
        List(
          GDef("natid", Pat("Z", List()), List(),
            TCtr("Z", List())),
          GDef("natid", Pat("S", List("pred")), List(),
            TCtr("S", List(TApp("natid", List(TVar("pred"))))))
        )
      }
    }

    it("add") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        def add(x: Nat, y: Nat): Nat = x match {
          case Z() => y
          case S(x1) => S(add(x1, y))
        }

      } should equal {
        List(
          GDef("add", Pat("Z", List()),List("y"),
            TVar("y")),
          GDef("add", Pat("S", List("x1")), List("y"),
            TCtr("S", List(TApp("add", List(TVar("x1"), TVar("y"))))))
        )

      }
    }

    it("append") {
      parseDefs {
        sealed trait List[A]
        case class Nil[A]() extends List[A]
        case class Cons[A](head: A, tail: List[A]) extends List[A]

        def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
          case Nil() => ys
          case Cons(x1, xs1) => Cons(x1, append(xs1, ys))
        }

      } should equal {
        List(
          GDef("append", Pat("Nil",List()), List("ys"),
            TVar("ys")),
          GDef("append", Pat("Cons", List("x1", "xs1")), List("ys"),
            TCtr("Cons", List(TVar("x1"), TApp("append", List(TVar("xs1"), TVar("ys"))))))
        )
      }
    }
  }
}

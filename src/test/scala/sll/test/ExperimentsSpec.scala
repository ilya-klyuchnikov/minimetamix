package sll.test

import common.adt._
import common.parser.parseDefs

import sll.data._
import sll.translator._
import sll.treeless._

class ExperimentsSpec extends org.scalatest.FunSpec with org.scalatest.Matchers {

  // Let's start with the simplest use case:
  sealed trait Step
  // Keep the element
  case class Keep() extends Step
  // Remove the element
  case class Remove() extends Step

  case class UPair[A](fst: A, snd: A)

  val prog1: Program =
    parseDefs {
      // interpreter
      def int[Elem](prog: List[Step], data: List[Elem]): List[Elem] = prog match {
        case Nil() => data
        case Cons(s, ss) => int1(data, s, ss)
      }

      def int1[Elem](data: List[Elem], s: Step, ss: List[Step]): List[Elem] = data match {
        case Nil() => Nil()
        case Cons(d, ds) => int2(s, ss, d, ds)
      }

      def int2[Elem](s: Step, ss: List[Step], d: Elem, ds: List[Elem]): List[Elem] = s match {
        case Keep() => Cons(d, int(ss, ds))
        case Remove() => int(ss, ds)
      }
    }

  val prog2: Program =
    parseDefs {
      def replace[Elem](data: List[Elem]): List[Nat] = data match {
        case Nil() => Nil()
        case Cons(e, es) => Cons(Z(), replace(es))
      }
    }

  val prog3: Program =
    parseDefs {
      def replace[Elem](data: List[Elem], n: Nat): List[Nat] = data match {
        case Nil() => Nil()
        case Cons(e, es) => Cons(n, replace(es, n))
      }
    }

  val prog4: Program =
    parseDefs {
      def zip1[A](xs: List[A], ys: List[A]): List[UPair[A]] = xs match {
        case Nil() => Nil()
        case Cons(x1, xs1) => zip12(ys, x1, xs1)
      }

      def zip12[A](ys: List[A], x1: A, xs1: List[A]): List[UPair[A]] = ys match {
        case Nil() => Nil()
        case Cons(y1, ys1) => Cons(UPair(x1, y1), zip1(xs1, ys1))
      }
    }

  val prog5: Program =
    parseDefs {
      def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
        case Nil() => ys
        case Cons(x1, xs1) => Cons(x1, append(xs1, ys))
      }

      def flatten[A](xss: List[List[A]]): List[A] = xss match {
        case Nil() => Nil()
        case Cons(xs1, xss1) => append(xs1, flatten(xss1))
      }
    }

  val prog6: Program =
    parseDefs {
      def flatten1[A](xss: List[List[A]]): List[A] = xss match {
        case Nil() => Nil()
        case Cons(xs1, xss1) => flatten12(xs1, xss1)
      }

      def flatten12[A](xs: List[A], xss: List[List[A]]): List[A] = xs match {
        case Nil() => flatten1(xss)
        case Cons(x1, xs1) => Cons(x1, flatten12(xs1, xss))
      }
    }

  describe("my program is OK") {
    it("prog1 is pure SLL") {
      validatePure(prog1)
    }

    it("prog2 is pure SLL") {
      validatePure(prog2)
    }

    it("prog3 is not pure SLL") {
      (the [AssertionError] thrownBy validatePure(prog3)).getMessage should include("pure linear: replace")
    }

    it("prog4 is good SLL") {
      validatePure(prog4)
    }

    it("prog5 (flatten) is not pure treeless") {
      (the [AssertionError] thrownBy validatePure(prog5)).getMessage should include("pure treeless: flatten")
    }

    it("prog6 (flatten1) is pure SLL") {
      validatePure(prog6)
    }
  }

}

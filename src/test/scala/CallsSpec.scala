package foetus.test

import foetus.ast._
import foetus.calls._
import foetus.parser._

class CallsSpec extends org.scalatest.FunSpec {

  describe("f, g, h, id, k") {
    val defs = parseDefs {
      sealed trait Nat
      case class Z() extends Nat
      case class S(pred: Nat) extends Nat
      def f(x: Nat): Nat = x match {
        case Z() => Z()
        case S(pred) => S(f(pred))
      }
      def g(x: Nat): Nat =
        g(x)
      def h(x: Nat): Nat =
        h(S(x))
      def id(x: Nat): Nat =
        x
      def k(x: Nat): Nat = id(x) match {
        case Z() => Z()
        case S(pred) => k(pred)
      }
    }

    assert(callGraph(defs) === List(
      Call("f", "f",  List(List(`<`))),
      Call("g", "g",  List(List(`=`))),
      Call("h", "h",  List(List(`?`))),
      Call("k", "id", List(List(`=`))),
      Call("k", "k",  List(List(`?`)))
    ))
  }
}

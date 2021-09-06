package foetus.test

import common.adt._
import common.parser._
import foetus.calls._

class CallsSpec extends org.scalatest.funspec.AnyFunSpec {

  describe("f, g, h, id, k") {
    val defs = parseDefs {
      def f(x: Nat): Nat = x match {
        case Z()     => Z()
        case S(pred) => S(f(pred))
      }
      def g(x: Nat): Nat =
        g(x)
      def h(x: Nat): Nat =
        h(S(x))
      def id(x: Nat): Nat =
        x
      def k(x: Nat): Nat = id(x) match {
        case Z()     => Z()
        case S(pred) => k(pred)
      }
    }

    assert(
      callGraph(defs) === List(
        Call("f", "f", List(List(`<`))),
        Call("g", "g", List(List(`=`))),
        Call("h", "h", List(List(`?`))),
        Call("k", "id", List(List(`=`))),
        Call("k", "k", List(List(`?`))),
      )
    )
  }
}

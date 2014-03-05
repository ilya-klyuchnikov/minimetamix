package foetus.test

import foetus.ast._
import foetus.calls._
import foetus.parser._

class CallsSpec extends org.scalatest.FunSpec with org.scalatest.Matchers {

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

    callGraph(defs) should equal { List(
      Call("f", "f", List(List(RelLess))),
      Call("g", "g", List(List(RelEqual))),
      Call("h", "h", List(List(RelUnknown))),
      Call("k", "id", List(List(RelEqual))),
      Call("k", "k", List(List(RelUnknown)))
    )
    }
  }

}


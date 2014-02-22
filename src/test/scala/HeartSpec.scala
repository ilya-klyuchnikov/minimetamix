package foetus.test

import org.scalatest._

class HeartSpec extends FunSpec with Matchers {
  import foetus.body._
  import foetus.heart._
  import foetus.parser._

  describe("f, g, h") {
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
    }

    analyseDefs(defs) should equal { List(
      Call("f", "f", List(List(RelLess))),
      Call("g", "g", List(List(RelEqual))),
      Call("h", "h", List(List(RelUnknown)))
    )
    }

  }

}


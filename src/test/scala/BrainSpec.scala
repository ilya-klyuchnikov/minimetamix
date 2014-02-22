package foetus.test

import org.scalatest._

class BrainSpec extends FunSpec with Matchers {
  import foetus.brain._
  import foetus.parser._

  describe("termination checker") {

    it ("add, mult") {
      val order = checkDefs(parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat
        def add(x: Nat, y: Nat): Nat = x match {
          case Z() => y
          case S(x1) => S(add(x1, y))
        }

        def mult(x: Nat, y: Nat): Nat = x match {
          case Z() => Z()
          case S(x1) => add(y, mult(x1, y))
        }

        def pred(x: Nat): Nat = x match {
          case Z() => Z()
          case S(x1) => x1
        }

        def sub(x: Nat, y: Nat): Nat = x match {
          case Z() => y
          case S(x1) => sub(x1, pred(y))
        }

        def div(x: Nat, y: Nat): Nat =
          div1(sub(pred(x), y), y)

        def div1(y: Nat, x: Nat): Nat = y match {
          case Z() => Z()
          case S(y1) => S(div1(sub(x, y), x))
        }
      })

      order should equal {
        List(
          "add" -> Some(List(0)),
          "mult" -> Some(List(0)),
          "pred" -> Some(List()),
          "sub" -> Some(List(0)),
          "div" -> Some(List()),
          "div1" -> None
        )
      }
    }

    it("ack") {
      val order = checkDefs(parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat
        def ack(x: Nat, y: Nat): Nat = x match {
          case Z() => S(y)
          case S(x1) => ack(x1, ack2(y, S(x1)))
        }

        def ack2(y: Nat, x: Nat): Nat = y match {
          case Z() => S(Z())
          case S(y1) => ack(x, y1)
        }
      })

      order should equal {
        List(
          "ack" -> None,
          "ack2" -> None
        )
      }

    }

  }
}

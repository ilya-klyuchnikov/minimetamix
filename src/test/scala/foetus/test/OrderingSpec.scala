package foetus.test

import common.adt._
import common.parser._
import foetus.ordering._

class OrderingSpec extends org.scalatest.FunSpec with org.scalatest.Matchers {

  describe("termination checker") {

    it ("add, mult, ...") {
      val order = orderDefs(parseDefs {

        def id0[A](x: A): A = x

        def loop[A](x: A): A = loop(x)

        def natid(x: Nat): Nat = x match {
          case Z() => Z()
          case S(x1) => S(natid(x1))
        }

        def natid1(x: Nat): Nat = x match {
          case Z() => Z()
          case S(x1) => S(natid1(S(x1)))
        }

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

        def listid[A](xs: List[A]): List[A] = xs match {
          case Nil() => Nil()
          case Cons(x1, xs1) => Cons(x1, listid(xs1))
        }

        // note this improvement! - it is accepted by
        def flatten[A](xss: List[List[A]]): List[A] = xss match {
          case Nil() => Nil()
          case Cons(xs, xss1) => xs match {
            case Nil() =>
              flatten(xss1)
            case Cons(x1, xs1) =>
              Cons(x1, flatten(Cons(xs1, xss1)))
          }
        }

        def le(x: Nat, y: Nat): Bool =
          x match {
            case Z() => True()
            case S(x1) => y match {
              case Z() => False()
              case S(y1) => le(x1, y1)
            }
          }

        def merge(l1: List[Nat], l2: List[Nat]): List[Nat] =
          l1 match {
            case Nil() => l2
            case Cons(x1, xs1) => l2 match {
              case Nil() => l1
              case Cons(y1, ys1) => le(x1, y1) match {
                case True() => Cons(x1, merge(xs1, l2))
                case False() => Cons(y1, merge(l1, ys1))
              }
            }
          }

        def zip[A](l1: List[A], l2: List[A]): List[A] =
          l1 match {
            case Nil() => l2
            case Cons(x1, xs1) => Cons(x1, zip(l2, xs1))
          }

        def zip1[A](l1: List[A], l2: List[A]): List[A] =
          l1 match {
            case Nil() => l2
            case Cons(x1, xs1) => Cons(x1, zip2(l2, xs1))
          }

        def zip2[A](l1: List[A], l2: List[A]): List[A] =
          l1 match {
            case Nil() => l2
            case Cons(x1, xs1) => Cons(x1, zip1(l2, xs1))
          }

      })

      order should equal {
        List(
          "id0" -> Some(List()),
          "loop" -> None,
          "natid" -> Some(List(0)),
          "natid1" -> None,
          "add" -> Some(List(0)),
          "mult" -> Some(List(0)),
          "pred" -> Some(List()),
          "sub" -> Some(List(0)),
          "div" -> Some(List()),
          "div1" -> None,
          "listid" -> Some(List(0)),
          "flatten" -> Some(List(0)),
          "le" -> Some(List(0)),
          "merge" -> Some(List(0, 1)),
          "zip" -> None,
          "zip1" -> Some(List(0)),
          "zip2" -> Some(List(0))
        )
      }
    }

    it("ack with aux function") {
      val order = orderDefs(parseDefs {
        def ack(x: Nat, y: Nat): Nat = x match {
          case Z() => S(y)
          case S(x1) => ack(x1, ack1(y, S(x1)))
        }

        def ack1(y: Nat, x: Nat): Nat = y match {
          case Z() => S(Z())
          case S(y1) => ack(x, y1)
        }
      })

      order should equal {
        List(
          "ack" -> Some(List(0, 1)),
          "ack1" -> Some(List(1, 0))
        )
      }

    }

    it("direct ack") {
      val order = orderDefs(parseDefs {
        def ack(x: Nat, y: Nat): Nat = x match {
          case Z() => S(y)
          case S(x1) => y match {
            case Z() => ack(x1, S(Z()))
            case S(y1) => ack(x1, ack(S(x1), y1))
          }
        }
      })

      order should equal {
        List(
          "ack" -> Some(List(0, 1))
        )
      }

    }

    it("fib") {
      val order = orderDefs(parseDefs{

        def add(x: Nat, y: Nat): Nat = x match {
          case Z() => y
          case S(x1) => S(add(x1, y))
        }

        def fib0(n: Nat, fn: Nat, fn0: Nat): Nat =
          n match {
            case Z() => fn
            case S(n1) => fib0(n1, add(fn, fn0), fn)
          }

        // fib = [n]fib’ n (S(O())) (O());
        def fib(n: Nat): Nat = fib0(n, S(Z()), Z())
      })

      order should equal {
        List(
          "add" -> Some(List(0)),
          "fib0" -> Some(List(0)),
          "fib" -> Some(List()))
      }
    }

  }
}

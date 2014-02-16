package foetus.test

import foetus.body._
import foetus.parser._
import org.scalatest._

class ParserSpec extends org.scalatest.FunSpec with Matchers {

  implicit def stringToVar(s: String) = TVar(s)
  implicit class TOps(t: Term) {
    def @@(t2: Term): Term = TApp(t, t2)
  }
  implicit class SOps(s: String) {
    def @@(t2: Term): Term = TApp(s, t2)
  }

  describe("parsing subset of Scala into foetus") {
    it("zero") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat
        val zero = Z()
      } should equal {
        List(
          "zero" -> TCtr("Z", TTup(List()))
        )
      }
    }

    it("one") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat
        val one = S(Z())
      } should equal {
        List(
          "one" -> TCtr("S", TCtr("Z", TTup(List())))
        )
      }
    }

    it("one via zero") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat
        val zero = Z()
        val one = S(zero)
      } should equal {
        List(
          "zero" -> TCtr("Z", TTup(List())),
          "one" -> TCtr("S", TVar("zero"))
        )
      }
    }

    it("id") {
      parseDefs {
        sealed trait Nat
        val id: Nat => Nat = {x => x}
      } should equal {
        List(
          "id" -> TLam("x", TVar("x"))
        )
      }
    }

    it("natid") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        val natid: Nat => Nat = x => x match {
          case Z() => Z()
          case S(pred) => S(natid(pred))
        }
      } should equal {
        List(
          "natid" -> TLam("x", TCase(TVar("x"),
            List(
              "Z" -> {"_" -> TCtr("Z", TTup(List()))},
              "S" -> {"pred" -> TCtr("S", TApp(TVar("natid"), TVar("pred")))})))
        )
      }
    }

    it("add") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        val add: Nat => Nat => Nat = x => y => x match {
          case Z() => y
          case S(x1) => S(add(x1)(y))
        }

      } should equal {
        List(
          "add" -> TLam("x", TLam("y",
            TCase(TVar("x"),
              List(
                "Z" -> { "_" -> TVar("y") },
                "S" -> { "x1" -> TCtr("S", TApp(TApp(TVar("add"), TVar("x1")), TVar("y"))) }))))
        )
      }
    }

    it("add, mult") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        val add: Nat => Nat => Nat = x => y => x match {
          case Z() => y
          case S(x1) => S(add(x1)(y))
        }
        val mult: Nat => Nat => Nat = x => y => x match {
          case Z() => Z()
          case S(x1) => add(y)(mult(x1)(y))
        }
      } should equal {
        List(
          "add" -> TLam("x", TLam("y",
            TCase(TVar("x"),
              List(
                "Z" -> { "_" -> TVar("y") },
                "S" -> { "x1" -> TCtr("S", TApp(TApp(TVar("add"), TVar("x1")), TVar("y"))) })))),
          "mult" -> TLam("x", TLam("y",
            TCase(TVar("x"),
              List(
                "Z" -> { "_" -> TCtr("Z", TTup(List())) },
                "S" -> { "x1" ->  TApp(TApp(TVar("add"), TVar("y")), TApp(TApp(TVar("mult"), TVar("x1")), TVar("y"))) }))))
        )
      }
    }

    it("pred") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        val pred: Nat => Nat = x => x match {
          case Z() => Z()
          case S(x1) => x1
        }

      } should equal {
        List(
          "pred" -> TLam("x", TCase(TVar("x"),
            List(
              "Z" -> {"_" -> TCtr("Z", TTup(List()))},
              "S" -> { "x1" -> TVar("x1") })))
        )
      }
    }

    it("sub") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        val pred: Nat => Nat = x => x match {
          case Z() => Z()
          case S(x1) => x1
        }

        val sub: Nat => Nat => Nat = x => y => x match {
          case Z() => y
          case S(x1) => sub(x1)(pred(y))
        }

      } should equal {
        List(
          "pred" -> TLam("x", TCase(TVar("x"),
            List(
              "Z" -> {"_" -> TCtr("Z", TTup(List()))},
              "S" -> { "x1" -> TVar("x1") }))),
          "sub" -> TLam("x", TLam("y",
            TCase(TVar("x"),
              List(
                "Z" -> { "_" -> TVar("y") },
                "S" -> { "x1" -> TApp(TApp(TVar("sub"), TVar("x1")), TApp(TVar("pred"), TVar("y"))) }))))
        )
      }
    }

    it("div") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        val pred: Nat => Nat = x => x match {
          case Z() => Z()
          case S(x1) => x1
        }

        val sub: Nat => Nat => Nat = x => y => x match {
          case Z() => y
          case S(x1) => sub(x1)(pred(y))
        }

        val div: Nat => Nat => Nat = x => y => {
          val div1: Nat => Nat = y1 => y1 match {
            case Z() => Z()
            case S(_) => S(div1(sub(x)(y1)))
          }
          div1(sub(pred(x))(y))
        }

      } should equal {
        List(
          "pred" -> TLam("x", TCase(TVar("x"),
            List(
              "Z" -> {"_" -> TCtr("Z", TTup(List()))},
              "S" -> { "x1" -> TVar("x1") }))),
          "sub" -> TLam("x", TLam("y",
            TCase(TVar("x"),
              List(
                "Z" -> { "_" -> TVar("y") },
                "S" -> { "x1" -> TApp(TApp(TVar("sub"), TVar("x1")), TApp(TVar("pred"), TVar("y"))) })))),
          "div" -> TLam("x", TLam("y",
            TLet(
              List("div1" -> TLam("y1", TCase(TVar("y1"),
                List(
                  "Z" -> { "_" -> TCtr("Z", TTup(List()))},
                  "S" -> { "_" -> TCtr("S", TApp(TVar("div1"), TApp(TApp(TVar("sub"), TVar("x")), TVar("y1")))) })
              ))),
              TApp(TVar("div1"), TApp(TApp(TVar("sub"), TApp(TVar("pred"), TVar("x"))), TVar("y"))))))
        )
      }
    }

    it("ack") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        def ack(x: Nat)(y: Nat): Nat = x match {
          case Z() => S(y)
          case S(x1) => ack(x1){y match {case Z() => S(Z()) case S(y1) => ack(x)(y1)}}
        }

      } should equal {
        List(
          "ack" -> TLam("x", TLam("y",
            TCase(TVar("x"),
              List(
                "Z" -> { "_" -> TCtr("S", TVar("y")) },
                "S" -> { "x1" ->
                  TApp(TApp(TVar("ack"), TVar("x1")),
                    TCase(TVar("y"),
                      List(
                        "Z" -> { "_" -> TCtr("S", TCtr("Z", TTup(List()))) },
                        "S" -> { "y1" -> TApp(TApp(TVar("ack"), TVar("x")), TVar("y1")) }))
                  )
                }))))
        )
      }
    }

    val tNil = TCtr("Nil", TTup(List()))
    def tCons(h: Term, t: Term) = TCtr("Cons", TTup(List("_1" -> h, "_2" -> t)))

    it("listid") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        sealed trait NatList
        case class Nil() extends NatList
        case class Cons(arg: (Nat, NatList)) extends NatList

        def listid(x: NatList): NatList = x match {
          case Nil() => Nil()
          case Cons(arg) => Cons(arg._1, listid(arg._2))
        }
      } should equal {
        List(
          "listid" -> TLam("x", TCase(TVar("x"),
            List(
              "Nil" -> { "_" -> tNil },
              "Cons" -> {
                "arg" -> tCons(TDot(TVar("arg"), "_1"), TApp(TVar("listid"), TDot(TVar("arg"), "_2")))
              })))
        )
      }
    }

    it("map") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        sealed trait NatList
        case class Nil() extends NatList
        case class Cons(arg: (Nat, NatList)) extends NatList
        // map = [f][list] let map1 = [l] case l of { Nil z => Nil() | Cons pair => Cons (HD=(f pair.HD), TL=(map1 pair.TL))} in (map1 list);
        def map(f: Nat => Nat)(list: NatList): NatList = {
          def map1(l: NatList): NatList = l match {
            case Nil() => Nil()
            case Cons(pair) => Cons(f(pair._1), map1(pair._2))
          }
          map1(list)
        }
      } should equal {
        List(
          "map" -> TLam("f", TLam("list",
            TLet(List(
              "map1" -> TLam("l",
                TCase(TVar("l"), List(
                  "Nil" ->  {"_" -> TCtr("Nil", TTup(List()))},
                  "Cons" -> {"pair" -> TCtr("Cons", TTup(List("_1" -> TApp(TVar("f"), TDot(TVar("pair"), "_1")), "_2" ->  TApp(TVar("map1"), TDot(TVar("pair"), "_2")) )))}
                )))
            ), TApp(TVar("map1"), TVar("list")))
          ))
        )
      }
    }

    it("foldl, rev") {
      parseDefs {
        sealed trait Nat
        case class Z() extends Nat
        case class S(pred: Nat) extends Nat

        sealed trait NatList
        case class Nil() extends NatList
        case class Cons(arg: (Nat, NatList)) extends NatList
        def cons(x: Nat)(y: NatList) = Cons(x, y)
        // foldl = [f][e][list] let foldl1 = [e][l] case l of { Nil z => e | Cons p => foldl1 (f p.HD e) p.TL } in foldl1 e list;
        def foldl(f: Nat => NatList => NatList)(e: NatList)(list: NatList): NatList = {
          def foldl1(e: NatList)(l: NatList): NatList = l match {
            case Nil() => e
            case Cons(p) => foldl1(f(p._1)(e))(p._2)
          }
          foldl1(e)(list)
        }
      } should equal {
        List(
          "cons" -> TLam("x", TLam("y", TCtr("Cons", TTup(List("_1" -> "x", "_2" -> "y"))))),
          "foldl" -> TLam("f", TLam("e", TLam("list",
            TLet(
              List(
                "foldl1" -> TLam("e", TLam("l", TCase("l",
                  List(
                    "Nil" -> {"_" -> "e"},
                    "Cons" -> {"p" -> "foldl1" @@ ("f" @@ TDot("p", "_1")@@ "e") @@ TDot("p", "_2")})
                )))),
              "foldl1" @@ "e" @@ "list"))))
        )
      }
    }

  }
}

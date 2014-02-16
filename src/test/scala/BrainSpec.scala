package foetus.test

class BrainSpec extends org.scalatest.FunSpec {
  import foetus.body._
  import foetus.brain._
  import foetus.parser._

  implicit def stringToVar(s: String) = TVar(s)
  implicit class TOps(t: Term) {
    def @@(t2: Term): Term = TApp(t, t2)
  }
  implicit class SOps(s: String) {
    def @@(t2: Term): Term = TApp(s, t2)
  }

  val zero = TCtr("Z", TTup(List()))
  val one = TCtr("S", zero)
  val two = TCtr("S", one)

  val zeroVal = VCtr("Z", VTup(List()))
  val oneVal = VCtr("S", zeroVal)
  val twoVal = VCtr("S", oneVal)

  val tNil = TCtr("Nil", TTup(List()))
  val vNil = VCtr("Nil", VTup(List()))
  def tCons(h: Term, t: Term) = TCtr("Cons", TTup(List("head" -> h, "tail" -> t)))
  def vCons(h: Val, t: Val) = VCtr("Cons", VTup(List("head" -> h, "tail" -> t)))

  val id0: Term =
    TLam("x", TVar("x"))

  val loop: Term =
    TLam("x", TApp(TVar("loop"), TVar("x")))

  val natid: Term =
    TLam("x", TCase(TVar("x"),
      List(
        "Z" -> { "z" -> zero },
        "S" -> { "x" -> TCtr("S", TApp(TVar("natid"), TVar("x"))) })))

  val natid1: Term =
    TLam("x", TCase(TVar("x"),
      List(
        "Z" -> { "z" -> zero },
        "S" -> { "x" -> TCtr("S", TApp(TVar("natid1"), TCtr("S", TVar("x")))) })))

  val add =
    TLam("x", TLam("y",
      TCase(TVar("x"),
        List(
          "Z" -> { "z" -> TVar("y") },
          "S" -> { "x1" -> TCtr("S", TApp(TApp(TVar("add"), TVar("x1")), TVar("y"))) }))))

  /*
  mult = [x][y]case x of
{ O z => O z
        | S x’ => (add y (mult x’ y)) };
   */

  val mult: Term =
    TLam("x", TLam("y",
      TCase(TVar("x"),
        List(
          "Z" -> { "z" -> zero },
          "S" -> { "x1" ->  TApp(TApp(TVar("add"), TVar("y")), TApp(TApp(TVar("mult"), TVar("x1")), TVar("y"))) }))))

  val listid: Term =
    TLam("x", TCase(TVar("x"),
      List(
        "Nil" -> { "_" -> tNil },
        "Cons" -> {
          "arg" -> tCons(TDot(TVar("arg"), "head"), TApp(TVar("listid"), TDot(TVar("arg"), "tail")))
        })))

  val y: Term =
    TLam("x", TApp(TVar("x"), TVar("x")))

  val pred: Term =
    TLam("x", TCase(TVar("x"),
      List(
        "Z" -> { "z" -> zero },
        "S" -> { "x" -> TVar("x") })))

  val sub =
    TLam("x", TLam("y",
      TCase(TVar("x"),
        List(
          "Z" -> { "z" -> TVar("y") },
          "S" -> { "x1" -> TApp(TApp(TVar("sub"), TVar("x1")), TApp(TVar("pred"), TVar("y"))) }))))

  // div = [x][y]let div1 = [y1]case y1 of { Z z => Z z | S dummy => S (div1 (sub x y1)) } in (div1 (sub (p x) y));
  val div: Term =
    TLam("x", TLam("y",
      TLet(
        List("div1" -> TLam("y1", TCase(TVar("y1"),
          List(
            "Z" -> { "z" -> zero },
            "S" -> { "dummy" -> TCtr("S", TApp(TVar("div1"), TApp(TApp(TVar("sub"), TVar("x")), TVar("y1")))) })
        ))),
        TApp(TVar("div1"), TApp(TApp(TVar("sub"), TApp(TVar("pred"), TVar("x"))), TVar("y")))))
    )

  // ack = [x][y]case x of {Z z => S y | S x1 => ack x1 (case y of {Z z => S (Z z) | S y1 => ack x y1} ) };
  val ack: Term =
    TLam("x", TLam("y",
      TCase(TVar("x"),
        List(
          "Z" -> { "_" -> TCtr("S", TVar("y")) },
          "S" -> { "x1" ->
            TApp(TApp(TVar("ack"), TVar("x1")),
              TCase(TVar("y"),
                List(
                  "Z" -> { "z" -> TCtr("S", TCtr("Z", TVar("z"))) },
                  "S" -> { "y1" -> TApp(TApp(TVar("ack"), TVar("x")), TVar("y1")) }))
            )
          }))))

  val ack1: Term =
    TLam("x", TLam("y",
      TCase(TVar("x"),
        List(
          "Z" -> { "z" -> TCtr("S", TVar("y")) },
          "S" -> { "x1" ->
            TApp(TApp(TVar("ack1"), TVar("x1")),
              TCase(TVar("y"),
                List(
                  "Z" -> { "z" -> TCtr("S", zero) },
                  "S" -> { "y1" -> TApp(TApp(TVar("ack1"), TVar("x")), TVar("y")) })) // should fail
            )
          }))))

  val map: Term =
    TLam("f", TLam("list",
      TLet(List(
        "map1" -> TLam("l",
          TCase(TVar("l"), List(
            "Nil" ->  {"z" -> TCtr("Nil", TTup(List()))},
            "Cons" -> {"pair" -> TCtr("Cons", TTup(List("HD" -> TApp(TVar("f"), TDot(TVar("pair"), "HD")), "TL" ->  TApp(TVar("map1"), TDot(TVar("pair"), "TL")) )))}
          )))
      ), TApp(TVar("map1"), TVar("list")))
    ))

  // foldl = [f][e][list] let foldl1 = [e][l] case l of { Nil z => e | Cons p => foldl1 (f p.HD e) p.TL } in foldl1 e list;

  val foldl: Term =
    TLam("f", TLam("e", TLam("list",
      TLet(
        List(
          "foldl1" -> TLam("e", TLam("l", TCase("l",
            List(
              "Nil" -> {"z" -> "e"},
              "Cons" -> {"p" -> "foldl1" @@ ("f" @@ TDot("p", "HD") @@ "e") @@ TDot("p", "TL")})
          )))),
        "foldl1" @@ "e" @@ "list"
      )
    )))

  val nil = tNil;
  val cons = TLam("h", TLam("t", TCtr("Cons", TTup(List("HD" -> "h", "TL" -> "t")))))

  // rev = [list] foldl cons nil list;
  val rev = TLam("list", "foldl" @@ "cons" @@ "nil" @@ "list")

  /**
   * flatten = [ll] case ll of
    { Nil z => Nil() | Cons p => case p.HD of
        { Nil z => flatten p.TL
        | Cons p1 => cons p1.HD (flatten(cons p1.TL p.TL)) }};
   */

  val flatten = TLam("ll", TCase("ll",
    List(
      "Nil" -> {"z" -> "nil"},
      "Cons" -> {"p" -> TCase(TDot("p", "HD"), List(
        "Nil" -> {"z" -> "flatten" @@ TDot("p", "TL")},
        "Cons" -> {"p1" -> "cons" @@ TDot("p1", "HD") @@ ("flatten" @@ ("cons" @@ TDot("p1", "TL") @@ TDot("p", "TL")))})
      )})))

  /**
   *
   * flat1 = [l]case l of { Nil z => nil | Cons p => aux p.HD p.TL },
   * aux = [l][ls] case l of { Nil z => flat1 ls | Cons p => cons p.HD (aux p.TL ls) };
   */

  val flat1 = TLam("l", TCase("l", List("Nil" -> {"z" -> "nil"}, "Cons" ->  {"p" -> "aux" @@ TDot("p", "HD") @@ TDot("p", "TL")})))
  val aux = TLam("l", TLam("ls",
    TCase("l", List("Nil" -> {"z" -> "flat1" @@ "ls"}, "Cons" ->  {"p" -> "cons" @@ TDot("p", "HD") @@ ("aux" @@ TDot("p", "TL") @@ "ls")}))))

  describe("termination checker") {

    it("id0") {
      val id0Check = checkDefs(Env(), List("id0" -> id0))
      assert(id0Check === Map("id0" -> Some(List())))
    }

    it("loop") {
      val loopCheck = checkDefs(Env(), List("loop" -> loop))
      assert(loopCheck === Map("loop" -> None))
    }

    it("natid") {
      val natIdCheck = checkDefs(Env(), List("natid" -> natid))
      assert(natIdCheck === Map("natid" -> Some(List(0))))
    }

    it("natid1") {
      val natId1Check = checkDefs(Env(), List("natid1" -> natid1))
      assert(natId1Check === Map("natid1" -> None))
    }

    it("add") {
      val addCheck = checkDefs(Env(), List("add" -> add))
      assert(addCheck === Map("add" -> Some(List(0))))
    }

    it("y") {
      val yCheck = checkDefs(Env(), List("y" -> y))
      assert(yCheck === Map("y" -> Some(List())))
    }

    it("listid") {
      val listIdCheck = checkDefs(Env(), List("listid" -> listid))
      assert(listIdCheck === Map("listid" -> Some(List(0))))
    }

    it("mult") {
      val addMultCheck = checkDefs(Env(), List("add" -> add, "mult" -> mult))
      assert(addMultCheck === Map("add" -> Some(List(0)), "mult" -> Some(List(0))))
    }

    it("pred") {
      val predSubCheck = checkDefs(Env(), List("pred" -> pred, "sub" -> sub))
      assert(predSubCheck === Map("pred" -> Some(List()), "sub" -> Some(List(0))))
    }

    it("pred, sub, div") {
      val predSubDivCheck = checkDefs(Env(), List("pred" -> pred, "sub" -> sub, "div" -> div))
      assert(predSubDivCheck === Map("pred" -> Some(List()), "sub" -> Some(List(0)), "div" -> Some(List()), "div1" -> None))
    }

    it("ack") {
      val ackCheck = checkDefs(Env(), List("ack" -> ack))
      assert(ackCheck === Map("ack" -> Some(List(0, 1))))
    }

    it("ack1") {
      val ack1Check = checkDefs(Env(), List("ack1" -> ack1))
      assert(ack1Check === Map("ack1" -> None))
    }

    it("map") {
      val mapCheck = checkDefs(Env(), List("map" -> map))
      assert(mapCheck === Map("map" -> Some(List()), "map1" -> Some(List(0))))
    }

    it("foldl") {
      val foldlCheck = checkDefs(Env(), List("foldl" -> foldl))
      assert(foldlCheck === Map("foldl" -> Some(List()), "foldl1" -> Some(List(1))))
    }

    it("rev") {
      val revCheck = checkDefs(Env(), List("nil" -> nil, "cons" -> cons, "foldl" -> foldl, "rev" -> rev))
      assert(revCheck === Map("cons" -> Some(List()), "nil" -> Some(List()), "rev" -> Some(List()), "foldl" -> Some(List()), "foldl1" -> Some(List(1))))
    }

    it("flatten") {
      val flattenCheck = checkDefs(Env(), List("nil" -> nil, "cons" -> cons, "flatten" -> flatten))
      assert(flattenCheck === Map("nil" -> Some(List()), "cons" -> Some(List()), "flatten" -> None))
    }

    it("flat1") {
      val flat1Check = checkDefs(Env(), List("nil" -> nil, "cons" -> cons, "flat1" -> flat1, "aux" -> aux))
      assert(flat1Check === Map("nil" -> Some(List()), "cons" -> Some(List()), "flat1" -> Some(List(0)), "aux" -> Some(List(1, 0))))
    }

    it("merge") {
      val mergeCheck = checkDefs(
        Env(), parseDefs{
          sealed trait Bool
          case class True() extends Bool
          case class False() extends Bool

          sealed trait Nat
          case class Z() extends Nat
          case class S(pred: Nat) extends Nat

          sealed trait NatList
          case class Nil() extends NatList
          case class Cons(arg: (Nat, NatList)) extends NatList

          val leNat: Nat => Nat => Bool = {x => y =>
            x match {
              case Z() => True()
              case S(x1) => y match {
                case Z() => False()
                case S(y1) => leNat(x1)(y1)
              }
            }
          }
          /*
          merge = [le][l1][l2]case l1 of
          { Nil z => l2
          | Cons p1 => case l2 of
                  { Nil z => l1
                  | Cons p2 => case (le p1.HD p2.HD) of
                          { True  z => Cons(HD=p1.HD,
                                       TL=merge le p1.TL l2)
                          | False z => Cons(HD=p2.HD,
                                       TL=merge le l1 p2.TL) }}};
           */
          val merge : (Nat => Nat => Bool) => (NatList) => NatList => NatList = { le => l1 => l2 =>
            l1 match {
              case Nil() => l2
              case Cons(p1) => l2 match {
                case Nil() => l1
                case Cons(p2) => le(p1._1)(p2._1) match {
                  case True() => Cons(p1._1, merge(le)(p1._2)(l2))
                  case False() => Cons(p2._1, merge(le)(l1)(p2._2))
                }
              }
            }
          }
        })

      assert(mergeCheck === Map("leNat" -> Some(List(0)), "merge" -> Some(List(1, 2))))
    }

    it("zip") {
      val zipCheck = checkDefs(
        Env(), parseDefs{
          sealed trait Nat
          case class Z() extends Nat
          case class S(pred: Nat) extends Nat

          sealed trait NatList
          case class Nil() extends NatList
          case class Cons(arg: (Nat, NatList)) extends NatList
          /*
          zip = [l1][l2]case l1 of
          { Nil z => l2
            | Cons p1 => Cons(HD=p1.HD, TL=zip l2 p1.TL) };
           */
          val zip : (NatList) => NatList => NatList = { l1 => l2 =>
            l1 match {
              case Nil() => l2
              case Cons(p1) => Cons(p1._1, zip(l2)(p1._2))
            }
          }
        })

      assert(zipCheck === Map("zip" -> None))
    }

    it("zip2") {
      val zip2Check = checkDefs(
        Env(), parseDefs{
          sealed trait Nat
          case class Z() extends Nat
          case class S(pred: Nat) extends Nat

          sealed trait NatList
          case class Nil() extends NatList
          case class Cons(arg: (Nat, NatList)) extends NatList
          /*
          zip = [l1][l2]case l1 of
          { Nil z => l2
            | Cons p1 => Cons(HD=p1.HD, TL=zip l2 p1.TL) };
           */
          val zip1 : (NatList) => NatList => NatList = { l1 => l2 =>
            l1 match {
              case Nil() => l2
              case Cons(p1) => Cons(p1._1, zip2(l2)(p1._2))
            }
          }
          val zip2 : (NatList) => NatList => NatList = { l1 => l2 =>
            l1 match {
              case Nil() => l2
              case Cons(p1) => Cons(p1._1, zip1(l2)(p1._2))
            }
          }
        })

      assert(zip2Check === Map("zip1" -> Some(List(0)), "zip2" -> Some(List(0))))
    }

    it("addOrd") {
      val addOrdCheck = checkDefs(
        Env(), parseDefs{
          trait Nat

          sealed trait Ord
          case class Z() extends Ord
          case class S(pred: Ord) extends Ord
          case class Lim(xx: Nat => Ord) extends Ord
          /*
          addord = [x][y]case x of
          { O o => y
          | S x’ => S(addord x’ y)
          | Lim f => Lim([z]addord (f z) y) };
           */
          val addOrd: Ord => Ord => Ord = { x => y =>
            x match {
              case Z() => y
              case S(x1) => S(addOrd(x1)(y))
              case Lim(f) => Lim({(z: Nat) => addOrd(f(z))(y)})
            }

          }
        })

      assert(addOrdCheck === Map("addOrd" -> Some(List(0))))
    }

    it("fib") {
      val fibCheck = checkDefs(
        Env(), parseDefs{
          sealed trait Nat
          case class Z() extends Nat
          case class S(pred: Nat) extends Nat

          def add(x: Nat)(y: Nat): Nat = x match {
            case Z() => y
            case S(x1) => S(add(x1)(y))
          }

          /*
          fib’ = [n][fn][fn’]case n of {Oz =>fn
          | S n’ => fib’ n’ (add fn fn’) fn};
          fib = [n]fib’ n (S(O())) (O());
           */
          val fib0: Nat => Nat => Nat => Nat = { n => fn => fn0 =>
            n match {
              case Z() => fn
              case S(n1) => fib0(n1)(add(fn)(fn0))(fn)
            }
          }
          // fib = [n]fib’ n (S(O())) (O());
          val fib: Nat => Nat = {n => fib0(n)(S(Z()))(Z())}
        })

      assert(fibCheck === Map("add" -> Some(List(0)), "fib0" -> Some(List(0)), "fib" -> Some(List())))
    }
  }
}

package foetus.test

import foetus.{brain, body}

object examples {
  import body._
  import brain._

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
            "S" -> { "dummy" -> TApp(TVar("div1"), TApp(TApp(TVar("sub"), TVar("x")), TVar("y1"))) })
        ))),
        TApp(TVar("div1"), TApp(TApp(TVar("sub"), TApp(TVar("pred"), TVar("x"))), TVar("y")))))
    )

  // ack = [x][y]case x of {Z z => S y | S x1 => ack x1 (case y of {Z z => S (Z z) | S y1 => ack x y1} ) };
  val ack: Term =
    TLam("x", TLam("y",
      TCase(TVar("x"),
        List(
          "Z" -> { "z" -> TCtr("S", TVar("y")) },
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
              "Cons" -> {"p" -> "foldl1" @@ ("f" @@ TDot("p", "HD")) @@ TDot("p", "TL")})
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

  def main(args: Array[String]) {

    val id0Check = checkDefs(Nil, List("id0" -> id0))
    assert(id0Check)

    val loopCheck = checkDefs(Nil, List("loop" -> loop))
    assert(!loopCheck)

    val natIdCheck = checkDefs(Nil, List("natid" -> natid))
    assert(natIdCheck)


    val natId1Check = checkDefs(Nil, List("natid1" -> natid1))
    assert(!natId1Check)

    val addCheck = checkDefs(Nil, List("add" -> add))
    assert(addCheck)

    val yCheck = checkDefs(Nil, List("y" -> y))
    assert(yCheck)

    val listIdCheck = checkDefs(Nil, List("listid" -> listid))
    assert(listIdCheck)

    val addMultCheck = checkDefs(Nil, List("add" -> add, "mult" -> mult))
    assert(addMultCheck)

    val predSubCheck = checkDefs(Nil, List("pred" -> pred, "sub" -> sub))
    assert(predSubCheck)

    val predSubDivCheck = checkDefs(Nil, List("pred" -> pred, "sub" -> sub, "div" -> div))
    assert(!predSubDivCheck)

    val ackCheck = checkDefs(Nil, List("ack" -> ack))
    assert(ackCheck)

    val ack1Check = checkDefs(Nil, List("ack1" -> ack1))
    assert(!ack1Check)

    val mapCheck = checkDefs(Nil, List("map" -> map))
    assert(mapCheck)

    val foldlCheck = checkDefs(Nil, List("foldl" -> foldl))
    assert(foldlCheck)

    val revCheck = checkDefs(Nil, List("nil" -> nil, "cons" -> cons, "foldl" -> foldl, "rev" -> rev))
    assert(revCheck)

    val flattenCheck = checkDefs(Nil, List("nil" -> nil, "cons" -> cons, "flatten" -> flatten))
    assert(!flattenCheck)

    val flat1Check = checkDefs(Nil, List("nil" -> nil, "cons" -> cons, "flat1" -> flat1, "aux" -> aux))
    assert(flat1Check)
  }
}

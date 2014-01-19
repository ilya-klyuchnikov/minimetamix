package foetus.test

import foetus.body._

class BodySpec extends org.scalatest.FunSpec {
  val zero = TCtr("Z", TTup(List()))
  val one = TCtr("S", zero)
  val two = TCtr("S", one)

  val zeroVal = VCtr("Z", VTup(List()))
  val oneVal = VCtr("S", zeroVal)
  val twoVal = VCtr("S", oneVal)

  val nil = TCtr("Nil", TTup(List()))
  val nilVal = VCtr("Nil", VTup(List()))
  def cons(h: Term, t: Term) = TCtr("Cons", TTup(List("head" -> h, "tail" -> t)))
  def consVal(h: Val, t: Val) = VCtr("Cons", VTup(List("head" -> h, "tail" -> t)))

  val id: Term =
    TLam("x", TVar("x"))

  val natid: Term =
    TLam("x", TCase(TVar("x"),
      List(
        "Z" -> {"z" -> zero},
        "S" -> {"x" -> TCtr("S", TApp(TVar("natid"), TVar("x")))})))

  val add: Term =
    TLam("x", TLam("y",
      TCase(TVar("x"),
        List(
          "Z" -> {"z" -> TVar("y")},
          "S" -> {"x1" -> TCtr("S", TApp(TApp(TVar("add"), TVar("x1")), TVar("y")))}))))

  val listid: Term =
    TLam("x", TCase(TVar("x"),
      List(
        "Nil" -> {"_" -> nil},
        "Cons" -> {"arg" -> cons(TDot(TVar("arg"), "head"), TApp(TVar("listid"), TDot(TVar("arg"), "tail")))
        })))

  def eval(bind: (String, Term), goal: Term): Val =
    nf(Clos(TLet(List(bind), goal), List()))

  describe("evaluator") {
    it("zero") {
      assert(eval("zero" -> zero, TVar("zero")) === zeroVal)
    }
    it("one") {
      assert(eval("one" -> one, TVar("one")) === oneVal)
    }
    it("two") {
      assert(eval("two" -> two, TVar("two")) === twoVal)
    }

    it("id zero") {
      assert(eval("id" -> id, TApp(TVar("id"), zero)) === zeroVal)
    }
    it("id one") {
      assert(eval("id" -> id, TApp(TVar("id"), one)) === oneVal)
    }

    it("natid zero") {
      assert(eval("natid" -> natid, TApp(TVar("natid"), zero)) === zeroVal)
    }
    it("natid one") {
      assert(eval("natid" -> natid, TApp(TVar("natid"), one)) === oneVal)
    }

    it("add one zero") {
      assert(eval("add" -> add, TApp(TApp(TVar("add"), one), zero)) === oneVal)
    }
    it("add one one") {
      assert(eval("add" -> add, TApp(TApp(TVar("add"), one), one)) === twoVal)
    }

    it("listid nil") {
      assert(eval("listid" -> listid, TApp(TVar("listid"), nil)) === nilVal)
    }
    it("listid (cons zero nil)") {
      assert(eval("listid" -> listid, TApp(TVar("listid"), cons(zero, nil))) === consVal(zeroVal, nilVal))
    }
  }
}

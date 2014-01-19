package foetus

// foetus.sml
// foetus "body"
// - data structures for terms (Tm), environments (Env), closures (Clos)
// - values
// - functions for evaluation of terms (hnf)
// - type aliases are in package.scala
object body {
  sealed trait Term
  case class TVar(n : String) extends Term
  case class TLam(arg : String, body : Term) extends Term
  case class TApp(h : Term, t : Term) extends Term
  case class TCtr(name : String, arg : Term) extends Term
  case class TCase(arg : Term, pats : List[Pat]) extends Term
  case class TTup(args: List[(String, Term)]) extends Term
  case class TDot(t: Term, field: String) extends Term
  case class TLet(defs: List[(String, Term)], t: Term) extends Term
  // C x => t
  type Pat = (String, (String, Term))
  type Defs = List[(String, Term)]

  // pay attention to closure - it is heavily used further
  case class Clos(t: Term, env : Env)
  type Env = List[Entry]
  sealed trait Entry
  // value of named variable
  case class EVal(v: String, clos: Clos) extends Entry
  // definitions
  case class ERec(defs: Defs) extends Entry
  // name generator
  case class EGen(v: String, i: Int) extends Entry

  sealed trait Val
  case class VCtr(name: String, arg: Val) extends Val
  case class VTup(fields: List[(String, Val)]) extends Val
  case class VLam(v: String, clos: Clos) extends Val
  case class VApp(v : Val, clos: Clos) extends Val
  case class VCase(v: Val, ptrs: List[Pat], env: Env) extends Val
  case class VGen(v: String, i: Int) extends Val
  case class VCtrLazy(name: String, arg: Clos) extends Val
  case class VTupLazy(fields: List[(String, Term)], env: Env) extends Val
  case class VDot(v: Val, name: String) extends Val
  case class VLet(defs: Defs, t: Term, env: Env) extends Val
  case class VDef(clos: Clos) extends Val

  // hnf' from foetus.sml
  def hnf0(lzy : Boolean, term: Term, rho: Env): Val = term match {
    case TVar(x) =>
      hnfVar(lzy, x, rho)
    case TApp(t, s) =>
      hnfApp(lzy, hnf0(lzy, t, rho), s, rho)
    case TCase(t, ps) =>
      hnfCase(lzy, hnf0(lzy, t, rho), ps, rho)
    case TDot(t, c) =>
      hnfDot(lzy, hnf0(lzy, t, rho), c)
    case TLet(xts, t) if lzy =>
      VLet(xts, t, rho)
    case TLet(xts, t) if !lzy =>
      hnf0(lzy, t, ERec(xts) :: rho)
    case TLam(x, t) =>
      VLam(x, Clos(t, rho))
    case TCtr(c, t) =>
      VCtrLazy(c, Clos(t, rho))
    case TTup(cts) =>
      VTupLazy(cts, rho)
  }

  def hnfVar(lzy: Boolean, x: String, env: Env): Val = env match {
    case Nil =>
      sys.error(s"undefined var $x")
    // "generic" variable
    case EGen(y, i) :: rho =>
      if (x == y) VGen(y, i) else hnfVar(lzy, x, rho)
    // "bound" variable
    case EVal(y, cl) :: rho =>
      if (x == y) hnf0(lzy, cl.t, cl.env) else hnfVar(lzy, x, rho)
    // defined variable
    case rho@(ERec(xts) :: rho1) =>
      xts.find(_._1 == x) match {
        case Some((_, t)) =>
          if (lzy) VDef(Clos(t, rho)) else hnf0(lzy, t, rho)
        case None =>
          hnfVar(lzy, x, rho1)
      }
  }

  def hnfApp(b: Boolean, value: Val, term: Term, env: Env): Val = value match {
    case VLam(x, Clos(t, rho)) =>
      hnf0(b, t, EVal(x, Clos(term, env)) :: rho)
    case v =>
      VApp(v, Clos(term, env))
  }

  def hnfCase(lzy: Boolean, value: Val, ps: List[Pat], rho: Env): Val = value match {
    case VCtrLazy(c, Clos(t, rho1)) =>
      val clause = ps.find(_._1 == c).getOrElse(sys.error("case not matched"))
      val (x, u) = clause._2
      hnf0(lzy, u, EVal(x, Clos(t, rho1)) :: rho)
    case v =>
      VCase(v, ps, rho)
  }

  def hnfDot(lzy: Boolean, value: Val, c: String): Val = value match {
    case VTupLazy(cts, rho) =>
      hnf0(lzy, cts.find(_._1 == c).get._2, rho)
    case v =>
      VDot(v, c)
  }

  def hnf(cl: Clos) =
    hnf0(true, cl.t, cl.env)
  def hnfEager(cl: Clos) =
    hnf0(false, cl.t, cl.env)

  def nf(cl: Clos): Val = hnfEager(cl) match {
    case VCtrLazy(name, clos) =>
      VCtr(name, nf(clos))
    case VTupLazy(cts, rho) =>
      VTup(cts.map({case (c, t) => (c, nf(Clos(t, rho)))}))
    case v =>
      v
  }
  
  var curId = 0
  def newGen(): Int = {
    curId += 1
    curId
  }
  def addGen(x: String, env: Env): Env =
    EGen(x, newGen) :: env
}

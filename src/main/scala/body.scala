package foetus

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

  case class Env(entries: List[Entry] = Nil) {
    def +(entry: Entry): Env =
      Env(entry +: entries)
    def findEntry(name: String): Option[Entry] =
      entries.find {
        case EVal(v, _) => v == name
        case EGen(v, _) => v == name
        case ERec(defs) => defs.exists(_._1 == name)
      }
    def addGen(x: String) =
      this + EGen(x, newGen())
  }

  sealed trait Entry
  // real value of argument, v - name of argument
  case class EVal(v: String, clos: Clos) extends Entry
  // "dummy" value of argument - when going into lambda - the same as ExtIdent
  case class EGen(v: String, i: Int) extends Entry
  case class ERec(defs: Defs) extends Entry


  sealed trait Val
  case class VCtr(name: String, arg: Val) extends Val
  case class VCtrLazy(name: String, arg: Clos) extends Val
  case class VTup(fields: List[(String, Val)]) extends Val
  case class VTupLazy(fields: List[(String, Term)], env: Env) extends Val
  case class VLam(v: String, clos: Clos) extends Val

  // with "neutral var"
  case class VApp(v : Val, clos: Clos) extends Val
  case class VCase(v: Val, ptrs: List[Pat], env: Env) extends Val

  case class VGen(v: String, i: Int) extends Val
  case class VDot(v: Val, name: String) extends Val
  // lazy let
  case class VLet(defs: Defs, t: Term, env: Env) extends Val

  // used during termination checking = normalization?
  def lazyNF(cl: Clos) =
    LazyNorm.norm(cl.t, cl.env)

  def eagerNF(cl: Clos): Val = EagerNorm.norm(cl.t, cl.env) match {
    case VCtrLazy(name, clos) =>
      VCtr(name, eagerNF(clos))
    case VTupLazy(cts, rho) =>
      VTup(cts.map({case (c, t) => (c, eagerNF(Clos(t, rho)))}))
    case v =>
      v
  }

  trait Norm {
    def norm(term: Term, rho: Env): Val = term match {
      case TVar(x) =>
        normVar(x, rho)
      case TApp(t, s) =>
        norm(t, rho) match {
          case VLam(x, Clos(t1, rho1)) =>
            norm(t1, rho1 + EVal(x, Clos(s, rho)))
          case v =>
            VApp(v, Clos(s, rho))
        }
      case TCase(t, ps) =>
        norm(t, rho) match {
          case VCtrLazy(c, Clos(t, rho1)) =>
            val clause = ps.find(_._1 == c).getOrElse(sys.error("case not matched"))
            val (x, u) = clause._2
            norm(u, rho + EVal(x, Clos(t, rho1)) )
          case v =>
            VCase(v, ps, rho)
        }
      case TDot(t, c) =>
        norm(t, rho) match {
          case VTupLazy(cts, rho1) =>
            norm(cts.find(_._1 == c).get._2, rho1)
          case v =>
            VDot(v, c)
        }
      case TLet(defs, t) =>
        normLet(defs, t, rho)
      case TLam(x, t) =>
        VLam(x, Clos(t, rho))
      case TCtr(c, t) =>
        VCtrLazy(c, Clos(t, rho))
      case TTup(cts) =>
        VTupLazy(cts, rho)
    }

    def normVar(x: String, env: Env): Val

    def normLet(defs: List[(String, Term)], t: Term, rho: Env): Val
  }

  object LazyNorm extends Norm {
    def normVar(x: String, env: Env): Val = env.findEntry(x).getOrElse(sys.error(s"undefined var $x")) match {
      // "generic" variable
      case EGen(_, i) =>
        VGen(x, i)
      // "bound" variable
      case EVal(_, cl) =>
        norm(cl.t, cl.env)
    }

    override def normLet(defs: List[(String, Term)], t: Term, rho: Env): Val =
      VLet(defs, t, rho)
  }

  object EagerNorm extends Norm {

    override def normVar(x: String, env: Env): Val = env.findEntry(x).getOrElse(sys.error(s"undefined var $x")) match {
      case EGen(_, i) =>
        VGen(x, i)
      // "bound" variable
      case EVal(_, cl) =>
        norm(cl.t, cl.env)
      // defined variable
      case ERec(defs) =>
        val (_, t) = defs.find(_._1 == x).get
        norm(t, env)
    }
    override def normLet(defs: List[(String, Term)], t: Term, rho: Env): Val =
      norm(t, rho + ERec(defs))
  }

  private var curId = 0
  def newGen(): Int = {
    curId += 1
    curId
  }
}

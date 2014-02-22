package foetus

object body {
  case class Def(name: String, params: List[String], body: Term)

  sealed trait Term {
    val size: Int
  }
  case class Var(n : String) extends Term {
    val size = 1
  }
  case class App(h : String, args : List[Term]) extends Term {
    val size = 1 + args.map(_.size).sum
  }
  case class Ctr(name : String, args : List[Term]) extends Term {
    val size = 1 + args.map(_.size).sum
  }
  case class Case(selector: Term, branches: List[Branch]) extends Term {
    val size = 1 + selector.size + branches.map(_.size).sum
  }
  case class Branch(pat: Pat, body: Term) {
    val size = 1 + pat.params.size + body.size
  }
  case class Pat(name: String, params: List[String])

  sealed trait Relation
  case object RelLess extends Relation {
    override def toString = "<"
  }
  case object RelEqual extends Relation {
    override def toString = "="
  }
  case object RelUnknown extends Relation {
    override def toString = "?"
  }

  def he(term1: Term, term2: Term): Boolean =
    heByDiving(term1, term2) || heByCoupling(term1, term2)

  private def heByDiving(term1: Term, term2: Term): Boolean = term2 match {
    case Ctr(_, args) => args exists (he(term1, _))
    case App(_, args) => args exists (he(term1, _))
    case _ => false
  }

  private def heByCoupling(term1: Term, term2: Term): Boolean = (term1, term2) match {
    case (Var(v1), Var(v2)) =>
      v1 == v2
    case (Ctr(n1, args1), Ctr(n2, args2)) =>
      n1 == n2 && (args1, args2).zipped.forall(he)
    case (App(n1, args1), App(n2, args2)) =>
      n1 == n2 && (args1, args2).zipped.forall(he)
    case _ => false
  }

  implicit class TermOps(t: Term) {
    def <=> (t2: Term): Relation =
      if (he(t, t2)) {
        if (t.size < t2.size) RelLess else RelEqual
      } else {
        RelUnknown
      }

    // elementary substitution
    def /(from: String, to: Term): Term = t match {
      case Var(v) =>
        if (v == from) to else Var(v)
      case Ctr(n, args) =>
        Ctr(n, args.map(_ / (from, to)))
      case App(n, args) =>
        App(n, args.map(_ / (from, to)))
      case Case(sel, bs) =>
        Case(sel / (from, to), bs map {case Branch(p, body) => Branch(p, body / (from, to))})
    }
  }

}

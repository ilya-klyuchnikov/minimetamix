package common

object ast {
  case class Def(name: String, params: List[String], body: Term)
  sealed trait Term
  case class Var(n : String) extends Term
  case class App(h : String, args : List[Term]) extends Term
  case class Ctr(name : String, args : List[Term]) extends Term
  case class Case(selector: Term, branches: List[Branch]) extends Term
  case class Branch(pat: Pat, body: Term)
  case class Pat(name: String, params: List[String])

  implicit class TermOps(val t: Term) {
    def size: Int = t match {
      case Var(_) =>        1
      case App(_, args) =>  1 + args.map(_.size).sum
      case Ctr(_, args) =>  1 + args.map(_.size).sum
      case Case(sel, bs) => 1 + sel.size + bs.map{b => 1 + b.pat.params.size + b.body.size}.sum
    }
  }
}

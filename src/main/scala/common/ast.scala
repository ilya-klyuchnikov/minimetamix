package common

object ast {
  case class Def(name: String, params: List[String], body: Term)
  sealed trait Term
  case class Var(n: String) extends Term
  case class App(h: String, args: List[Term]) extends Term
  case class Ctr(name: String, args: List[Term]) extends Term
  case class Case(selector: Term, branches: List[Branch]) extends Term
  case class Branch(pat: Pat, body: Term)
  case class Pat(name: String, params: List[String])
}

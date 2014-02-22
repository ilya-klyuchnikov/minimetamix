package foetus

object body {
  sealed trait Term
  case class TVar(n : String) extends Term
  case class TApp(h : String, t : List[Term]) extends Term
  case class TCtr(name : String, args : List[Term]) extends Term

  trait Def {
    val name: String
  }
  case class FDef(name: String, args: List[String], body: Term) extends Def
  case class GDef(name: String, pat: Pat, args: List[String], body: Term) extends Def

  case class Pat(name: String, params: List[String])
}

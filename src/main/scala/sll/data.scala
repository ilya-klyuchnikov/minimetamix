package sll

/**
  * SLL AST (without case expressions)
  */
object data {
  type Name = String

  sealed trait Expr
  case class Var(n: Name) extends Expr
  case class Ctr(name: Name, args: List[Expr]) extends Expr
  case class FCall(name: Name, args: List[Expr]) extends Expr
  case class GCall(name: Name, args: List[Expr]) extends Expr

  case class Pat(name: Name, params: List[Name])

  sealed trait Def {
    val name: Name
    val body: Expr
  }
  case class FDef(name: Name, params: List[Name], body: Expr) extends Def
  case class GDef(name: Name, pat: Pat, params: List[Name], body: Expr) extends Def

  case class Program(defs: List[Def])
}

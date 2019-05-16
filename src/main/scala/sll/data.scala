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

  case class FDef(name: Name, params: List[Name], body: Expr)
  case class GDef(name: Name, pat: Pat, params: List[Name], body: Expr)

  case class Program(fDefs: List[FDef], gDefs: List[GDef])
}

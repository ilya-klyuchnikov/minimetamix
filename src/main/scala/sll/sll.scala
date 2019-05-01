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
  case class Let(k: Name, v: Expr, body: Expr) extends Expr

  case class Pat(name: Name, params: List[Name])

  case class FDef(name: Name, params: List[Name], body: Expr)
  case class GDef(name: Name, pat: Pat, params: List[Name], body: Expr)

  case class Program(fDefs: List[FDef], gDefs: List[GDef])
}

/**
  * Translation of Foetus AST (with case expressions) into SLL AST (without case expressions)
  */
object translator {

  import foetus.ast.{Var => F_Var, Ctr => F_Ctr, Case, App, Def, Term}
  import data._

  implicit def translate(defs: List[Def]): Program = {
    val gNames = defs.collect { case Def(n, _, Case(_, _)) => n }.toSet

    def translateDef(d: Def): Program = d match {
      case Def(n, g_param :: params, Case(selector, bs)) if selector == F_Var(g_param) =>
        val gDefs = bs.map { branch =>
          GDef(n, Pat(branch.pat.name, branch.pat.params), params, translateExpr(branch.body))
        }
        Program(Nil, gDefs)
      case Def(n, params, body) =>
        Program(FDef(n, params, translateExpr(body)) :: Nil, Nil)
    }

    def translateExpr(term: Term): Expr = term match {
      case F_Var(n) => Var(n)
      case App(n, args) =>
        if (gNames(n)) GCall(n, args.map(translateExpr)) else FCall(n, args.map(translateExpr))
      case F_Ctr(n, args) => Ctr(n, args.map(translateExpr))
    }

    defs.map(translateDef).reduce((p1, p2) => Program(p1.fDefs ++ p2.fDefs, p1.gDefs ++ p2.gDefs))
  }
}

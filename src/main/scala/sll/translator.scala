package sll

import scala.language.implicitConversions

/**
  * Translation of Foetus AST (with case expressions) into SLL AST (without case expressions)
  */
object translator {

  import common.ast.{Var => F_Var, Ctr => F_Ctr, Case, App, Def, Term}
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

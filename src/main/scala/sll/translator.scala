package sll

import scala.language.implicitConversions

/**
  * Translation of Foetus AST (with case expressions) into SLL AST (without case expressions)
  */
object translator {

  import common.ast.{Var => F_Var, Ctr => F_Ctr, Case, App, Def => F_Def, Term}
  import data._

  implicit def translate(defs: List[F_Def]): Program = {
    val gNames = defs.collect { case F_Def(n, _, Case(_, _)) => n }.toSet

    def translateDef(d: F_Def): List[Def] = d match {
      case F_Def(n, g_param :: params, Case(selector, bs)) if selector == F_Var(g_param) =>
        val gDefs = bs.map { branch =>
          GDef(n, Pat(branch.pat.name, branch.pat.params), params, translateExpr(branch.body))
        }
        gDefs
      case F_Def(n, params, body) =>
        List(FDef(n, params, translateExpr(body)))
    }

    def translateExpr(term: Term): Expr = term match {
      case F_Var(n) => Var(n)
      case App(n, args) =>
        if (gNames(n)) GCall(n, args.map(translateExpr)) else FCall(n, args.map(translateExpr))
      case F_Ctr(n, args) => Ctr(n, args.map(translateExpr))
    }

    Program(defs.map(translateDef).reduce(_ ++ _))
  }
}

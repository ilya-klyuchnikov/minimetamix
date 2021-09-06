package common

import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

import common.ast._

object parser {
  def parseDefs(expr: Any): List[Def] = macro parseDefsImpl

  def parseDefsImpl(c: blackbox.Context)(expr: c.Expr[Any]): c.Expr[List[Def]] = {
    import c.universe._

    object Name {
      def unapply(name: Name): Option[String] =
        Some(name.decodedName.toString)
    }

    def parseTree(tree: Tree): Term = tree match {
      case Ident(Name(name)) =>
        Var(name)
      case Match(selector, cases) =>
        Case(parseTree(selector), cases.map(parseCaseDef))
      case Apply(Select(ctr, Name("apply")), args) =>
        Ctr(unname(ctr), args.map(parseTree))
      case Apply(TypeApply(Select(ctr, Name("apply")), _), args) =>
        Ctr(unname(ctr), args.map(parseTree))
      case Apply(Ident(Name(name)), args) =>
        App(name, args.map(parseTree))
      case Apply(TypeApply(Ident(Name(ctrName)), _), args) =>
        App(ctrName, args.map(parseTree))
      case Apply(Select(_, Name(name)), args) =>
        App(name, args.map(parseTree))
      case Apply(TypeApply(Select(_, Name(name)), _), args) =>
        App(name, args.map(parseTree))
    }

    def parseCaseDef(caze: CaseDef): Branch = caze.pat match {
      case Apply(tt: TypeTree, bs) =>
        Branch(
          Pat(unname(tt.original), bs.map { case Bind(Name(n), _) => n }),
          parseTree(caze.body),
        )
    }

    def unname(t: Tree): String = t match {
      case Ident(Name(name))     => name
      case Select(_, Name(name)) => name
    }

    def parseStmt(tree: Tree): Def = tree match {
      case DefDef(modifiers, Name(n), _, params, _, rhs) =>
        Def(n, params.head.map(_.name.decodedName.toString), parseTree(rhs))
    }

    def parseBlock(tree: Tree): c.Expr[List[Def]] = tree match {
      case Block(stmts, _) =>
        val defs = stmts.map(parseStmt)
        reify(qList(defs.map(qDef(_))).splice)
    }

    def qs(s: String) =
      c.Expr[String](Literal(Constant(s)))

    def qList[T: WeakTypeTag](xs: List[Expr[T]]): Expr[List[T]] = {
      val nil = reify { List[T]() }
      xs.foldRight(nil) { (x, y) => reify { x.splice :: y.splice } }
    }

    def qTerm(t: Term): Expr[Term] = t match {
      case Var(n) =>
        reify {
          Var(qs(n).splice)
        }
      case Ctr(n, args) =>
        reify {
          Ctr(qs(n).splice, qList(args.map(qTerm(_))).splice)
        }
      case App(n, args) =>
        reify {
          App(qs(n).splice, qList(args.map(qTerm(_))).splice)
        }
      case Case(sel, branches) =>
        reify {
          Case(qTerm(sel).splice, qList(branches.map(qBranch(_))).splice)
        }
    }

    def qBranch(b: Branch): Expr[Branch] = b match {
      case Branch(Pat(n, ps), body) =>
        reify {
          Branch(Pat(qs(n).splice, qList(ps.map(qs(_))).splice), qTerm(body).splice)
        }
    }

    def qDef(d: Def): Expr[Def] = d match {
      case Def(n, ps, body) =>
        reify {
          Def(qs(n).splice, qList(ps.map(qs(_))).splice, qTerm(body).splice)
        }
    }

    parseBlock(expr.tree)
  }
}

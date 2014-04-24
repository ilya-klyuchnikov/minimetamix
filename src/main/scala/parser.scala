package foetus

import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros._

import ast._

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
      case Apply(Select(Ident(Name(ctrName)), Name("apply")), args) =>
        Ctr(ctrName, args.map(parseTree))
      case Apply(TypeApply(Select(Ident(Name(ctrName)), Name("apply")), tpParams), args) =>
        Ctr(ctrName, args.map(parseTree))
      case Apply(Ident(Name(name)), args) =>
        App(name, args.map(parseTree))
      case Apply(TypeApply(Ident(Name(ctrName)), _), args) =>
        App(ctrName, args.map(parseTree))
      case x =>
        c.abort(tree.pos, "FOETUS. Unsupported syntax")
    }

    def parseCaseDef(caze: CaseDef): Branch = caze.pat match {
      case UnApply(Apply(Select(Ident(Name(name)), _), _), bs) =>
        Branch(Pat(name, bs.map { case Bind(Name(n), _) => n }), parseTree(caze.body))
      case UnApply(Apply(TypeApply(Select(Ident(Name(name)), _), _), _), bs) =>
        Branch(Pat(name, bs.map { case Bind(Name(n), _) => n }), parseTree(caze.body))
      case x =>
        c.abort(caze.pat.pos, "FOETUS. Unsupported syntax")
    }

    def parseDef(name: String, vparamss: List[List[ValDef]], body: Tree): List[Def] = vparamss match {
      case params :: Nil =>
        List(Def(name, params.map(_.name.decodedName.toString), parseTree(body)))
      case _ =>
        c.abort(wrappingPos(vparamss.head), "FOETUS. Unsupported syntax")
    }

    def parseStmt(tree: Tree): List[Def] = tree match {
      case DefDef(modifiers, name, typeParams, params, returnType, body) =>
        parseDef(name.decodedName.toString, params, body)
      case x =>
        //c.warning(tree.pos, "FOETUS. Ignored definition")
        Nil
    }

    def parseStmts(stmts: List[Tree]): List[Def] =
      stmts.flatMap(parseStmt)

    def parseBlock(tree: Tree): c.Expr[List[Def]] = tree match {
      case Block(stmts, _) =>
        val defs = parseStmts(stmts)
        reify(qList(defs.map(qDef(_))).splice)
    }

    def qs(s: String) =
      c.Expr[String](Literal(Constant(s)))

    def qList[T: WeakTypeTag](xs: List[Expr[T]]): Expr[List[T]] =
      xs.foldRight(reify{ Nil: List[T] }) {
        (x, y) => reify { x.splice :: y.splice }
      }

    def qTerm(t: Term): Expr[Term] = t match {
      case Var(n) =>
        reify {
          Var(qs(n).splice)
        }
      case Ctr(n, args) =>
        reify {
          Ctr(qs(n).splice, qList(args.map(qTerm(_))).splice )
        }
      case App(n, args) =>
        reify {
          App(qs(n).splice, qList(args.map(qTerm(_))).splice )
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
          Def(qs(n).splice, qList(ps.map(qs(_))).splice , qTerm(body).splice)
        }
    }

    parseBlock(expr.tree)
  }
}

package foetus

import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros._

import ast._

object parser {
  def parseDefs(expr: Any): List[Def] = macro parseDefsImpl

  def parseDefsImpl(c: Context)(expr: c.Expr[Any]): c.Expr[List[Def]] = {
    import c.universe._

    object Name {
      def unapply(name: Name): Option[String] = Some(name.decoded)
    }

    def trim(s: String) = s.split("\n").toList match {
      case s1 :: Nil => s1
      case s1 :: ss => s"$s1..."
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
        sys.error(s"UNKNOWN: ${showRaw(tree)}")
    }

    def parseCaseDef(caze: CaseDef): Branch = caze.pat match {
      case Apply(ctr: TypeTree, bs) =>
        val Ident(Name(name)) = ctr.original
        Branch(Pat(name, bs.map {case Bind(Name(n), _) => n}), parseTree(caze.body))
      case x =>
        sys.error(s"UNKNOWN: ${showRaw(caze.pat)}")
    }

    def parseDef(name: String, vparamss: List[List[ValDef]], body: Tree): List[Def] = vparamss match {
      case params :: Nil =>
        List(Def(name, params.map{_.name.decoded}, parseTree(body)))
      case x =>
        sys.error("curried functions are not supported in SLL")
    }

    def parseStmt(tree: Tree): List[Def] = tree match {
      case DefDef(modifiers, name, typeParams, params, returnType, body) =>
        parseDef(name.decoded, params, body)
      case x =>
        //println(s"WARNING ignoring unsupported definition: `${trim(tree.toString())}`")
        Nil
    }

    def parseStmts(stmts: List[Tree]): List[Def] =
      stmts.flatMap(parseStmt)

    def parseBlock(tree: Tree): c.Expr[List[Def]] =  tree match {
      case Block(stmts, _) =>
        val defs = parseStmts(stmts)
        reify(qListDef(defs.map(d => qDef(d))).splice)
    }

    def qs(s: String): Expr[String] =
      c.Expr[String](Literal(Constant(s)))

    def qListString(xs: List[Expr[String]]): Expr[List[String]] =
      xs.foldRight(reify{Nil: List[String]}){ (x, y) => reify{x.splice :: y.splice}}

    def qListDef(xs: List[Expr[Def]]): Expr[List[Def]] =
      xs.foldRight(reify{Nil: List[Def]}){ (x, y) => reify{x.splice :: y.splice}}

    def qListTerm(xs: List[Expr[Term]]): Expr[List[Term]] =
      xs.foldRight(reify{Nil: List[Term]}){ (x, y) => reify{x.splice :: y.splice}}

    def qListBranch(xs: List[Expr[Branch]]): Expr[List[Branch]] =
      xs.foldRight(reify{Nil: List[Branch]}){ (x, y) => reify{x.splice :: y.splice}}

    def qTerm(t: Term): Expr[Term] = t match {
      case Var(n) =>
        reify {
          Var(qs(n).splice)
        }
      case Ctr(n, args) =>
        reify {
          Ctr(qs(n).splice, qListTerm(args.map(t => qTerm(t))).splice )
        }
      case App(n, args) =>
        reify {
          App(qs(n).splice, qListTerm(args.map(t => qTerm(t))).splice )
        }
      case Case(sel, branches) =>
        reify {
          Case(qTerm(sel).splice, qListBranch(branches.map(b => qBranch(b))).splice)
        }
    }

    def qBranch(b: Branch): Expr[Branch] = b match {
      case Branch(Pat(n, ps), body) =>
        reify {
          Branch(Pat(qs(n).splice, qListString(ps.map({ s => qs(s) })).splice), qTerm(body).splice)
        }
    }

    def qDef(d: Def): Expr[Def] = d match {
      case Def(n, ps, body) =>
        reify {
          Def(qs(n).splice, qListString(ps.map({ s => qs(s) })).splice , qTerm(body).splice)
        }
    }

    parseBlock(expr.tree)
  }
}

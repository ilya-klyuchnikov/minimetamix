package foetus

import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros._

import body._

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
      // x | Ident(newTermName("x"))
      case Ident(Name(name)) =>
        TVar(name)
      // Z.apply()
      case Apply(Select(Ident(Name(ctrName)), Name("apply")), args) =>
        TCtr(ctrName, args.map(parseTree))
      // Cons.apply[A](x, xs1) |
      case Apply(TypeApply(Select(Ident(Name(ctrName)), Name("apply")), tpParams), args) =>
        TCtr(ctrName, args.map(parseTree))
      // natid(pred)
      case Apply(Ident(Name(name)), args) =>
        TApp(name, args.map(parseTree))
      // listId[A](xs1)
      case Apply(TypeApply(Ident(Name(ctrName)), _), args) =>
        TApp(ctrName, args.map(parseTree))
      case x =>
        //println(tree)
        sys.error(s"UNKNOWN: ${showRaw(tree)}")
    }

    def parseCaseDef(caze: CaseDef): (Pat, Term) = caze.pat match {
      case Apply(ctr: TypeTree, bs) =>
        val Ident(Name(name)) = ctr.original
        (Pat(name, bs.map {case Bind(Name(n), _) => n}), parseTree(caze.body))
      case x =>
        sys.error(s"UNKNOWN: ${showRaw(caze.pat)}")
    }

    def parseRHS(name: String, params: List[String], rhs: Tree): List[Def] = rhs match {
      // TODO - check that selector is the first variable
      case Match(Ident(_), cases) =>
        for { c <- cases; (pat, body) = parseCaseDef(c) } yield GDef(name, pat, params.tail, body)
      case _ =>
        List(FDef(name, params, parseTree(rhs)))
    }

    def parseDef(name: String, vparamss: List[List[ValDef]], body: Tree): List[Def] = vparamss match {
      case params :: Nil =>
        parseRHS(name, params.map{_.name.decoded}, body)
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

    def qTerm(t: Term): Expr[Term] = t match {
      case TVar(n) =>
        reify{ TVar(qs(n).splice) }
      case TCtr(n, args) =>
        reify{ TCtr(qs(n).splice, qListTerm(args.map(t => qTerm(t))).splice ) }
      case TApp(n, args) =>
        reify{ TApp(qs(n).splice, qListTerm(args.map(t => qTerm(t))).splice ) }
    }

    def qDef(d: Def): Expr[Def] = d match {
      case FDef(n, ps, body) =>
        reify{ FDef(qs(n).splice, qListString(ps.map({ s => qs(s) })).splice , qTerm(body).splice )}
      case GDef(n, Pat(cn, ps1), ps2, body) =>
        reify{GDef(
          qs(n).splice,
          Pat(qs(cn).splice, qListString(ps1.map({ s => qs(s) })).splice),
          qListString(ps2.map({ s => qs(s) })).splice,
          qTerm(body).splice)}
    }

    parseBlock(expr.tree)
  }
}

package foetus

import language.implicitConversions
import scala.language.experimental.macros

import scala.reflect.macros._
import body._

object parser {
  def parseDefs(expr: Any): List[(String, Term)] = macro parseDefsImpl

  def parseDefsImpl(c: Context)(expr: c.Expr[Any]): c.Expr[List[(String, Term)]] = {
    val parser = new parser[c.type](c, debug = false)
    parser.parseBlock(expr.tree)
  }
}

class parser[C <: Context](val c: C, debug: Boolean) {
  import c.universe._

  private implicit class TreeHelper(tree: Tree) {
    def raw: String = showRaw(tree)
  }

  private object Name {
    def unapply(name: Name): Option[String] = Some(name.decoded)
  }

  private def trim(s: String) = s.split("\n").toList match {
    case s1 :: Nil => s1
    case s1 :: ss => s"$s1..."
  }

  private def stringLit(s: String): Expr[String] =
    c.Expr[String](Literal(Constant(s)))

  private def parseTree(tree: Tree): Expr[Term] = {
    //println()
    //println(s"-- parsing $tree")
    //println(s"++ parsing ${tree.raw}")
    tree match {
      case Function(List(param), body) =>
        reify{ TLam(stringLit(param.name.decoded).splice, parseTree(body).splice) }
      case Select(from, field) =>
        reify{ TDot(parseTree(from).splice, stringLit(field.decoded).splice) }
      case Match(selector, cases) =>
        reify{ TCase(parseTree(selector).splice, parseCaseDefs(cases).splice) }
      // `v`
      case Ident(name) =>
        reify{ TVar(stringLit(name.decoded).splice) }
      // zero-arg constructor like `Z()`
      case Apply(Select(Ident(Name(ctrName)), Name("apply")), List()) =>
        reify{ TCtr(stringLit(ctrName).splice, TTup(List())) }
      // tuple-apply like Tuple2(x, y), Tuple2(x, y, z)
      case Apply(Select(Ident(Name(ctrName)), Name("apply")), List(Apply(TypeApply(_), tupleArgs))) =>
        reify{ TCtr(stringLit(ctrName).splice, TTup(parseTupleArgs(1, tupleArgs).splice)) }
      // one-arg constructor
      case Apply(Select(Ident(Name(ctrName)), Name("apply")), List(arg)) if ctrName(0).isUpper =>
        reify{ TCtr(stringLit(ctrName).splice, parseTree(arg).splice) }
      // application of higher-order function
      case Apply(Select(t, Name("apply")), List(arg)) =>
        reify{ TApp(parseTree(t).splice, parseTree(arg).splice) }
      // all calls are curried so far `f a`
      case Apply(operator, List(operand)) =>
        reify{ TApp(parseTree(operator).splice, parseTree(operand).splice) }
      case Block(defs, body) =>
        reify{ TLet(parseDefs(defs).splice, parseTree(body).splice) }
      case x =>
        //println(tree)
        sys.error(s"UNKNOWN: ${tree.raw}")
    }}

  private def parseCaseDef(caze: CaseDef): Expr[Pat] = {
    //println(caze.pat)
    //println(caze.pat.raw)
    caze.pat match {
      case Apply(ctr: TypeTree, List()) =>
        //println(ctr.raw)
        ctr.original match {
          case Ident(name) =>
            reify{ (stringLit(name.decoded).splice, ("_", parseTree(caze.body).splice)) }
        }
      case Apply(ctr: TypeTree, List(Bind(bname, _))) =>
        //println(ctr.raw)
        ctr.original match {
          case Ident(name) =>
            reify{ (stringLit(name.decoded).splice, (stringLit(bname.decoded).splice, parseTree(caze.body).splice)) }
        }
      // wilcard??
      case Apply(ctr: TypeTree, List(Ident(_))) =>
        //println(ctr.raw)
        ctr.original match {
          case Ident(name) =>
            reify{ (stringLit(name.decoded).splice, ("_", parseTree(caze.body).splice)) }
        }
      case x =>
        //println(caze.pat)
        sys.error(s"UNKNOWN: ${caze.pat.raw}")
    }
  }

  private def parseCaseDefs(cases: List[CaseDef]): Expr[List[Pat]] = cases match {
    case Nil =>
      reify(Nil)
    case caze::cazes =>
      reify{ parseCaseDef(caze).splice :: parseCaseDefs(cazes).splice }

  }

  private def parseTupleArgs(i: Int, args: List[Tree]): Expr[List[(String, Term)]] = args match {
    case Nil =>
      reify{ Nil }
    case x :: xs =>
      reify{ parseTupleArg(i, x).splice :: parseTupleArgs(i + 1, xs).splice }
  }

  private def parseTupleArg(i: Int, arg: Tree): Expr[(String, Term)] =
    reify{ (stringLit(s"_$i").splice, parseTree(arg).splice) }

  private def parseDef0(vparamss: List[List[ValDef]], body: Tree): Expr[Term] = vparamss match {
    case Nil =>
      parseTree(body)
    case List(param) :: params =>
      reify{ TLam(stringLit(param.name.decoded).splice, parseDef0(params, body).splice) }
  }

  private def parseDef(tree: Tree): Option[Expr[(String, Term)]] = tree match {
    case DefDef(modifiers, name, typeParams, params, returnType, body) =>
      Some(reify{ (stringLit(name.decoded).splice, parseDef0(params, body).splice) })
    case ValDef(modifiers, name, tp, body) =>
      Some(reify{ (stringLit(name.decoded).splice, parseTree(body).splice) })
    // type definition
    case x =>
      //println(s"WARNING ignoring unsupported definition: `${trim(tree.toString())}`")
      None
  }

  private def parseDefs(stmts: List[Tree]): Expr[List[(String, Term)]] = stmts match {
    case Nil =>
      reify{ Nil }
    case d :: ds =>
      parseDef(d) match {
        case None =>
          parseDefs(ds)
        case Some(expr) =>
          reify{ expr.splice :: parseDefs(ds).splice }
      }
  }

  def parseBlock(tree: Tree): c.Expr[List[(String, Term)]] =  tree match {
    case Block(defs, _) =>
      parseDefs(defs)
  }

}

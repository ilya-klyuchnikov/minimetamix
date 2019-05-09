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

/**
  * Utilities for manipulating SLL AST
  */
object utils {

  import data._

  def isValue(e: Expr): Boolean = e match {
    case Ctr(_, args) => args.forall(isValue)
    case _ => false
  }

  def isVar(e: Expr): Boolean = e match {
    case Var(_) => true
    case _ => false
  }

  def fDef(program: Program, fName: Name): FDef =
    program.fDefs.find(_.name == fName).get

  def gDefs(program: Program, gName: Name): List[GDef] =
    program.gDefs.filter(_.name == gName)

  def gDef(program: Program, gName: Name, pName: Name): GDef =
    gDefs(program, gName).find(_.pat.name == pName).get

  def names(e: Expr): List[Name] = e match {
    case Var(n) => List(n)
    case Ctr(_, args) => args.flatMap(names)
    case GCall(_, args) => args.flatMap(names)
    case FCall(_, args) => args.flatMap(names)
  }

  def isSllProgram(program: Program): Boolean =
    program.fDefs.forall(isSllFDef) && program.gDefs.forall(isSllGDef)

  // F-functions are sll-correct by constructions
  def isSllFDef(fDef: FDef): Boolean = true

  // G-functions can use the first parameter in many places.
  // This can result into undefined variables after translation.
  // Checking that all variables are defined.
  def isSllGDef(gDef: GDef): Boolean = {
    val params = gDef.pat.params ::: gDef.params
    val bodyVariables = names(gDef.body)
    bodyVariables.toSet.subsetOf(params.toSet)
  }

  def assertSll(program: Program): Unit = {
    for (fDef <- program.fDefs)
      assert(isSllFDef(fDef), s"SLL: ${fDef.name}")
    for (gDef <- program.gDefs)
      assert(isSllGDef(gDef), s"SLL: ${gDef.name}")
  }
}

object constraints {
  import data._
  import utils._

  def isPureTreeless(e: Expr): Boolean = e match {
    case Var(_) => true
    case Ctr(_, args) => args.forall(isPureTreeless)
    case FCall(_, args) => args.forall(isVar)
    case GCall(_, args) => args.forall(isVar)
  }

  def isPureLinear(e: Expr): Boolean = {
    val vars = names(e)
    vars.distinct == vars
  }

  def assertPureTreeless(program: Program): Unit = {
    for (fDef <- program.fDefs)
      assert(isPureTreeless(fDef.body), s"pure treeless: ${fDef.name}")
    for (gDef <- program.gDefs)
      assert(isPureTreeless(gDef.body), s"pure treeless: ${gDef.name}")
  }

  def assertPureLinear(program: Program): Unit = {
    for (fDef <- program.fDefs)
      assert(isPureLinear(fDef.body), s"pure linear: ${fDef.name}")
    for (gDef <- program.gDefs)
      assert(isPureLinear(gDef.body), s"pure linear: ${gDef.name}")
  }

  def validatePure(program: Program): Unit = {
    assertSll(program)
    assertPureTreeless(program)
    assertPureLinear(program)
  }
}

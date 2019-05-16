package sll

object treeless {
  import data._
  import utils._

  // pure treeless form

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

  // blazed treeless form

  def names_+(e: Expr): List[Name] = e match {
    case Var(n) => List(n)
    case Ctr(_, args) => args.flatMap(names)
    case GCall(_, args) => args.flatMap(names)
    case FCall("blaze", args) => Nil
    case FCall(_, args) => args.flatMap(names)
  }

  def isLinear_+(e: Expr): Boolean = {
    val vars = names_+(e)
    vars.distinct == vars
  }

  def is_-(e: Expr): Boolean = e match {
    case Var(_) => true
    case FCall("blaze", _) => true
    case _ => false
  }

  def isTreeless_+(e: Expr): Boolean = e match {
    case Var(_) => true
    case Ctr(_, args) => args.forall(isTreeless_+)
    case FCall(_, args) => args.forall(is_-)
    case GCall(_, args) => args.forall(is_-)
  }

  def assertTreeless_+(program: Program): Unit = {
    for (fDef <- program.fDefs)
      assert(isTreeless_+(fDef.body), s"blazed treeless: ${fDef.name}")
    for (gDef <- program.gDefs)
      assert(isTreeless_+(gDef.body), s"blazed treeless: ${gDef.name}")
  }

  def assertLinear_+(program: Program): Unit = {
    for (fDef <- program.fDefs)
      assert(isLinear_+(fDef.body), s"blazed linear: ${fDef.name}")
    for (gDef <- program.gDefs)
      assert(isLinear_+(gDef.body), s"blazed linear: ${gDef.name}")
  }

  def validate_+(program: Program): Unit = {
    assertSll(program)
    assertTreeless_+(program)
    assertLinear_+(program)
  }
}

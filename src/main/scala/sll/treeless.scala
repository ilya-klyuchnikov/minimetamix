package sll

object treeless {
  import data._
  import utils._

  // pure treeless form

  def isTreeless(e: Expr): Boolean = e match {
    case Var(_)         => true
    case Ctr(_, args)   => args.forall(isTreeless)
    case FCall(_, args) => args.forall(isVar)
    case GCall(_, args) => args.forall(isVar)
  }

  def isLinear(e: Expr): Boolean = {
    val vars = names(e)
    vars.distinct == vars
  }

  def assertTreeless(program: Program): Unit = {
    for (d <- program.defs)
      assert(isTreeless(d.body), s"pure treeless: ${d.name}")
  }

  def assertLinear(program: Program): Unit = {
    for (d <- program.defs)
      assert(isLinear(d.body), s"pure linear: ${d.name}")
  }

  def validate(program: Program): Unit = {
    assertSll(program)
    assertTreeless(program)
    assertLinear(program)
  }

  // blazed treeless form

  def names_+(e: Expr): List[Name] = e match {
    case Var(n)               => List(n)
    case Ctr(_, args)         => args.flatMap(names)
    case GCall(_, args)       => args.flatMap(names)
    case FCall("blaze", args) => Nil
    case FCall(_, args)       => args.flatMap(names)
  }

  def isLinear_+(e: Expr): Boolean = {
    val vars = names_+(e)
    vars.distinct == vars
  }

  def is_-(e: Expr): Boolean = e match {
    case Var(_)            => true
    case FCall("blaze", _) => true
    case _                 => false
  }

  def isTreeless_+(e: Expr): Boolean = e match {
    case Var(_)         => true
    case Ctr(_, args)   => args.forall(isTreeless_+)
    case FCall(_, args) => args.forall(is_-)
    case GCall(_, args) => args.forall(is_-)
  }

  def assertTreeless_+(program: Program): Unit = {
    for (d <- program.defs)
      assert(isTreeless_+(d.body), s"blazed treeless: ${d.name}")
  }

  def assertLinear_+(program: Program): Unit = {
    for (d <- program.defs)
      assert(isLinear_+(d.body), s"blazed linear: ${d.name}")
  }

  def validate_+(program: Program): Unit = {
    assertSll(program)
    assertTreeless_+(program)
    assertLinear_+(program)
  }
}

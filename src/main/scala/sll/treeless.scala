package sll

object treeless {
  import data._
  import utils._

  // pure treeless form

  def isTreeless(e: Expr): Boolean = e match {
    case Var(_) => true
    case Ctr(_, args) => args.forall(isTreeless)
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
    for (d <- program.defs)
      assert(isTreeless_+(d.body), s"blazed treeless: ${d.name}")
  }

  def validate_+(program: Program): Unit = {
    assertSll(program)
    assertTreeless_+(program)
    assertLinear(program)
  }

  def findUnOrderedFDef(d: Def): Option[Name] = d match {
    case FDef(fn, params, body) =>
      if (params == names(body)) None
      else Some(fn)
    case GDef(_, _, _, _) =>
      None
  }

  // the order of names in the lhs is the same as in the rhs
  def findUnOrderedFDefs(program: Program): List[Name] =
    program.defs.flatMap(findUnOrderedFDef)

  def findUnOrderedGDef(d: Def): Option[(Name, Name)] = d match {
    case FDef(_, _, _) => None
    case GDef(gn, Pat(pn, xs), ys, body) =>
      if ((xs ++ ys) == names(body))  None
      else Some((gn, pn))
  }

  def findUnOrderedGDefs(program: Program): List[(Name, Name)] =
    program.defs.flatMap(findUnOrderedGDef)


}

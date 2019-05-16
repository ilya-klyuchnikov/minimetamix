package sll

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

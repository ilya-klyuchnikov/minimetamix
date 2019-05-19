package sll

import sll.data._

object pprinter {

  private def pprintExpr(e: Expr): String = e match {
    case Var(n) => n
    case Ctr(n, args) => args.map(pprintExpr).mkString(s"$n(", ", ", ")")
    case FCall("blaze", List(arg)) => s"[${pprintExpr(arg)}]"
    case FCall(n, args) => args.map(pprintExpr).mkString(s"$n(", ", ", ")")
    case GCall(n, args) => args.map(pprintExpr).mkString(s"$n(", ", ", ")")
  }

  private def pprintPat(p: Pat): String =
    p.params.mkString(s"${p.name}(", ", ", ")")

  private def pprintF(fd: FDef): String = {
    val lhs = fd.params.mkString(s"${fd.name}(", ", ", ")")
    val rhs = pprintExpr(fd.body)
    s"$lhs = $rhs;"
  }

  private def pprintG(gd: GDef): String = {
    val params = pprintPat(gd.pat) :: gd.params
    val lhs = params.mkString(s"${gd.name}(", ", ", ")")
    val rhs = pprintExpr(gd.body)
    s"$lhs = $rhs;"
  }

  def pprintProgram(p: Program): String = {
    val defs = p.defs.map {
      case fDef: FDef => pprintF(fDef)
      case gDef: GDef => pprintG(gDef)
    }
    defs.mkString("\n")
  }

}

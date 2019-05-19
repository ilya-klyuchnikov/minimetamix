package sll

import sll.data._

object pprinter {

  private def pprintExpr(e: Expr): String = e match {
    case Var(n) => n
    case Ctr(n, args) => args.map(pprintExpr).mkString(s"$n(", ", ", ")")
    case FCall("blaze", List(arg)) => s"*${pprintExpr(arg)}"
    case FCall(n, args) => args.map(pprintExpr).mkString(s"$n(", ", ", ")")
    case GCall(n, args) => args.map(pprintExpr).mkString(s"$n(", ", ", ")")
  }

  private def pprintPat(p: Pat): String =
    p.params.mkString(s"${p.name}(", ", ", ")")

  private def pprintF(fd: FDef): (String, String) = {
    val lhs = fd.params.mkString(s"${fd.name}(", ", ", ")")
    val rhs = pprintExpr(fd.body)
    (lhs, rhs)
  }

  private def pprintG(gd: GDef): (String, String) = {
    val params = pprintPat(gd.pat) :: gd.params
    val lhs = params.mkString(s"${gd.name}(", ", ", ")")
    val rhs = pprintExpr(gd.body)
    (lhs, rhs)
  }

  def pprintDef(d: Def): (String, String) = d match {
    case fDef: FDef => pprintF(fDef)
    case gDef: GDef => pprintG(gDef)
  }

  def pprintProgram(p: Program): String = {
    val defs = p.defs.groupBy(_.name)
    val names = p.defs.map(_.name).distinct

    val parts: Seq[List[(String, String)]] =
      names.map(defs(_).map(pprintDef))

    val leftMax =
      parts.flatten.map(_._1.length).max

    val alignedParts = parts.map { _.map {
      case (lhs, rhs) =>
        val delta = " " * (leftMax - lhs.length)
        (lhs + delta, rhs)
    }}

    alignedParts.map(_.map { case (lhs, rhs) => s"$lhs = $rhs" }.mkString("\n")).mkString("\n\n")
  }

}

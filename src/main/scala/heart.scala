package foetus

import ast._
import body._

object heart {

  type Ident = String

  type CallMatrix = List[List[Relation]]

  case class Call(caller: String, callee: String, matr: CallMatrix) {
    override def toString: String = s"${caller} -> ${callee} | ${matr}"
  }

  def analyseDefs(defs: List[Def]): List[Call] = defs.flatMap {
    case Def(name, params, body) =>
      analyseBody(name, params.map(Var), body)
  }

  private def analyseBody(caller: Ident, params: List[Term], t: Term): List[Call] = t match {
    case Var(_) =>
      Nil
    case App(callee, args) =>
      val callMatrix = buildCallMat(params, args)
      val call = Call(caller, callee, callMatrix)
      call :: args.flatMap(analyseBody(caller, params, _))
    case Ctr(c, args) =>
      args.flatMap(analyseBody(caller, params, _))
    case Case(Var(n), bs) =>
      bs.flatMap { case Branch(Pat(ctrN, xs), body) =>
        val params1 = params map{ _ / (n, Ctr(ctrN, xs.map(Var))) }
        val body1 = body / (n, Ctr(ctrN, xs.map(Var)))
        analyseBody(caller, params1, body1)
      }
    case Case(sel, bs) =>
      analyseBody(caller, params, sel) ++ bs.flatMap(b => analyseBody(caller, params, b.body))
  }

  // callee params - should be terms
  // args - should be terms
  private def buildCallMat(params: List[Term], args: List[Term]): CallMatrix =
    args.map( arg => params.map (param => arg <=> param) )

}

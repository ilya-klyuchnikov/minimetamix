package foetus

import foetus.ast._

object calls {
  type CallGraph = List[Call]
  type CallMatrix = List[List[Relation]]

  case class Call(caller: String, callee: String, callMatrix: CallMatrix) {
    override def toString: String = s"${caller} -> ${callee} | ${callMatrix}"
  }

  /**
   * Builds a list of calls for given definitions.
   *
   * @param defs definitions to analyse
   * @return a list of calls f->g (for each call of g inside f)
   */
  def callGraph(defs: List[Def]): CallGraph = defs.flatMap {
    case Def(name, params, body) =>
      collectCalls(name, params.map(Var), body)
  }

  private def collectCalls(caller: String, params: List[Term], t: Term): List[Call] = t match {
    case Var(_) =>
      Nil
    case App(callee, args) =>
      val call = Call(caller, callee, callMatrix(params, args))
      call :: args.flatMap(collectCalls(caller, params, _))
    case Ctr(c, args) =>
      args.flatMap(collectCalls(caller, params, _))
    // propagation of positive information
    case Case(Var(vn), bs) =>
      bs.flatMap { case Branch(Pat(cn, xs), body) =>
        val params1 = params map{ _ / (vn, Ctr(cn, xs.map(Var))) }
        val body1 = body / (vn, Ctr(cn, xs.map(Var)))
        collectCalls(caller, params1, body1)
      }
    // no propagation =>
    // a link from function parameter -> pattern var is lost
    case Case(sel, bs) =>
      collectCalls(caller, params, sel) ++ bs.flatMap(b => collectCalls(caller, params, b.body))
  }

  // callee params - should be terms
  // args - should be terms
  private def callMatrix(callerParams: List[Term], calleeArgs: List[Term]): CallMatrix =
    calleeArgs.map( arg => callerParams.map (param => arg <=> param) )

  sealed trait Relation
  case object `=` extends Relation
  case object `<` extends Relation
  case object `?` extends Relation

  def he(term1: Term, term2: Term): Boolean =
    heByDiving(term1, term2) || heByCoupling(term1, term2)

  private def heByDiving(term1: Term, term2: Term): Boolean = term2 match {
    case Ctr(_, args) => args exists (he(term1, _))
    case App(_, args) => args exists (he(term1, _))
    case _ => false
  }

  private def heByCoupling(term1: Term, term2: Term): Boolean = (term1, term2) match {
    case (Var(v1), Var(v2)) =>
      v1 == v2
    case (Ctr(n1, args1), Ctr(n2, args2)) =>
      n1 == n2 && (args1, args2).zipped.forall(he)
    case (App(n1, args1), App(n2, args2)) =>
      n1 == n2 && (args1, args2).zipped.forall(he)
    case _ => false
  }

  implicit class TermOpsExtra(t: Term) {
    def <=> (t2: Term): Relation = (he(t, t2), t.size < t2.size) match {
      case (true, true) => `<`
      case (true, false) => `=`
      case (false, _) => `?`
    }

    // elementary substitution
    def /(bind: (String, Term)): Term = t match {
      case Var(vn) =>
        if (vn == bind._1) bind._2 else Var(vn)
      case Ctr(n, args) =>
        Ctr(n, args.map(_ / bind))
      case App(n, args) =>
        App(n, args.map(_ / bind))
      case Case(sel, bs) =>
        Case(sel / bind, bs map {case Branch(p, body) => Branch(p, body / bind)})
    }
  }
}

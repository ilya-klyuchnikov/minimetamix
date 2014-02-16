package foetus

import body._

object heart {

  type Ident = Int
  val emptyIdent: Ident = 0
  // the same as case class EGen(v: String, i: Int) extends Entry
  case class ExtIdent(name: String, id: Ident)
  val emptyExtIdent: ExtIdent = ExtIdent("$empty$", emptyIdent)

  sealed trait Relation
  case object RelLess extends Relation {
    override def toString = "<"
  }
  case object RelEqual extends Relation {
    override def toString = "="
  }
  case object RelUnknown extends Relation {
    override def toString = "?"
  }

  case class Dependencies private(map: Map[Ident, Set[Ident]] = Map().withDefaultValue(Set())) {
    def addLess(x: Ident, y: Ident): Dependencies =
      Dependencies(map.updated(x, map(x) + y))

    def getRel(x: Ident, y: Ident): Relation =
      if (x == y) RelEqual
      else if (map(x)(y)) RelLess
      else RelUnknown
  }

  object Dependencies {
    val empty = Dependencies()
  }

  /**
   * getRelation
   *
   * compares value v with variable par using the stored deps and returns
   * rel_less, rel_equal or rel_unknown.
   *
   * Considered cases:
   * 1. x = C y   => y < x  (C constructor)
   * 2. x R v.L   <= x R v  (L label, R in <, =, ?)
   * 3. x R (v a) <= x R v  (a argument(s))
   *
   * Not-yet considered cases:
   * 1. tupels
   * 2. further evaluation of v
   */
  private def getRelation(deps: Dependencies, v: Val, par: Ident): Relation = v match {
    case VGen(x, i) => deps.getRel(i, par)
    case VDot(v, c) => getRelation(deps, v, par)
    case VApp(v, cl) => getRelation(deps, v, par)
    case _ => RelUnknown
  }

  type CallMatrix = List[List[Relation]]
  case class FunHead(extId: ExtIdent, params: List[Ident])
  case class StaticEnv(caller: FunHead, dependencies: Dependencies)
  case class Call(caller: ExtIdent, callee: ExtIdent, matr: CallMatrix) {
    override def toString: String = s"${caller} -> ${callee} | ${matr}"
  }

  def analyseDefs(se: StaticEnv, env: Env, defs: Defs): List[Call] =
    analyseLet(se, env, defs, None)

  private def extractFunHead(fname: ExtIdent, v: Val, pars: List[Ident] = Nil): (FunHead, Val) = v match {
    case VLam(x, Clos(t, env)) =>
      val i = newGen()
      extractFunHead(fname, lazyNF(Clos(t,  env + EGen(x, i))), pars ++ List(i))
    case v =>
      (FunHead(fname, pars), v)
  }

  /**
   * creates a call from a function to "nothing" out of its head:
   *
   * to assure that also non-recursive functions appear in the output,
   * every defined expression is added to the Call list as "empty call".
   * Thus getCallers gets also non-recursive functions.
   */
  private def dummyCall(funHead: FunHead): Call =
    Call(funHead.extId, emptyExtIdent, Nil)

  // static env is not empty for inner defs
  // setup environment and launches analyze body
  private def analyseLet(se: StaticEnv, env: Env, defs: Defs, tm: Option[Term]): List[Call] = {

    // unique ids
    val extDefs: List[(ExtIdent, Term)] =
      defs.map { case (x, t) => (ExtIdent(x, newGen()), t) }
    val eGens: List[Entry] =
      extDefs.map { case (ExtIdent(x, i), _) => EGen(x, i) }
    val extEnv: Env =
      eGens.foldLeft(env)(_ + _)

    // builds normal form of definitions and extract "args of expression (lambdas)"
    val funs: List[(FunHead, Val)] =
      extDefs.map {case (x, t) => extractFunHead(x, lazyNF(Clos(t, extEnv)))}

    val defCalls: List[Call] = funs.flatMap {case (caller, v) =>
      dummyCall(caller) :: analyseBody(StaticEnv(caller, se.dependencies), v)
    }
    val bodyCalls: List[Call] =
      tm.map({t => analyseBody(se, lazyNF(Clos(t, extEnv)))}).getOrElse(Nil)

    defCalls ++ bodyCalls
  }

  // just collects - v - lazy value
  private def analyseBody(se: StaticEnv, v: Val): List[Call] = v match {
    case VApp(v, clos) =>
      analyseApp(se, v, args = List(clos))
    case vg: VGen =>
      analyseCall(se, vg, Nil)
    case VCtr(c, v) =>
      analyseBody(se, v)
    case VCtrLazy(c, cl) =>
      analyseBody(se, lazyNF(cl))
    case VTup(fields) =>
      fields.flatMap { case (_, v) => analyseBody(se, v) }
    case VTupLazy(cts, env) =>
      cts.flatMap{ case (_, t) => analyseBody(se, lazyNF(Clos(t, env))) }
    case VDot(v, _) =>
      analyseBody(se, v)
    case VCase(v, pats, env) =>
      analyseCases(analyseBody, se, v, pats, env)
    case VLet(defs, tm, env) =>
      analyseLet(se, env, defs, Some(tm))
    case VLam(x, Clos(t, env)) =>
      analyseBody(se, lazyNF(Clos(t, env.addGen(x))))
  }

  private def analyseCases(f: (StaticEnv, Val) => List[Call], se: StaticEnv, v: Val, pats: List[Pat], env: Env): List[Call] = v match {
    // selector is a variable
    case VGen(x, id) =>
      pats.flatMap(analyseCase(f, se, env, id, _: Pat))
    // selector is not a var -- nothing can be determined
    case _ =>
      analyseBody(se, v) ++ pats.flatMap(analyseCaseNoDep(f, se, env, _: Pat))
  }

  private def analyseCase(f: (StaticEnv, Val) => List[Call], se: StaticEnv, env: Env, id: Ident, pat: Pat): List[Call] = {
    val (_, (x, t)) = pat
    val id1 = newGen
    // TODO - dependency is added
    f(StaticEnv(se.caller, se.dependencies.addLess(id1, id)), lazyNF(Clos(t,  env + EGen(x, id1))))
  }

  private def analyseCaseNoDep(f: (StaticEnv, Val) => List[Call], se: StaticEnv, env: Env, pat: Pat): List[Call] = {
    val (_, (x, t)) = pat
    f(se, lazyNF(Clos(t, env.addGen(x))))
  }

  private def analyseApp(se: StaticEnv, head: Val, args: List[Clos]): List[Call] = head match {
    case vg: VGen =>
      analyseCall(se, vg, args)
    case VApp(v, cl) =>
      analyseApp(se, v, cl :: args)
    // TODO: no such case in SLL, since no HOF
    case VCase(v, pats, env) =>
      analyseCases({ (se1, v1) => analyseApp(se1, v1, args) }, se, v, pats, env)
    case _ =>
      sys.error(s"analyseApp: ${head}")
  }

  // vgen args inside a caller
  private def analyseCall(se: StaticEnv, vgen: VGen, args: List[Clos]): List[Call] = {
    val argVals = args.map(lazyNF)
    val argsCalls = argVals.flatMap(analyseBody(se, _))
    // AHA - very conservative!
    val callMatrix = buildCallMat(se.dependencies, se.caller.params, argVals)
    Call(se.caller.extId, ExtIdent(vgen.v, vgen.i), callMatrix) :: argsCalls
  }

  private def buildCallMat(deps: Dependencies, callerParams: List[Ident], calleeArgs: List[Val]): CallMatrix =
    calleeArgs.map {v => callerParams.map(getRelation(deps, v, _: Ident))}

  val emptyStaticEnv: StaticEnv =
    StaticEnv(FunHead(emptyExtIdent, Nil), Dependencies.empty)

}

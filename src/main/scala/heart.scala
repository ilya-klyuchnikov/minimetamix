package foetus

import body._

object heart {

  type Ident = String

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

  private def getRelation(deps: Dependencies, v: Term, par: Ident): Relation = v match {
    case TVar(i) => deps.getRel(i, par)
    case _ => RelUnknown
  }

  type CallMatrix = List[List[Relation]]
  case class FunHead(name: String, params: List[Ident])

  case class StaticEnv(caller: FunHead, dependencies: Dependencies)
  case class Call(caller: String, callee: String, matr: CallMatrix) {
    override def toString: String = s"${caller} -> ${callee} | ${matr}"
  }

  def analyseDefs(defs: List[Def]): List[Call] = defs.flatMap {
    case FDef(name, params, body) =>
      analyseBody(StaticEnv(FunHead(name, params), Dependencies.empty), body)
    case GDef(name, Pat(_, params1), params2, body) =>
      val params = "$" :: params2
      val deps = params1.foldLeft(Dependencies.empty)(_.addLess(_, "$"))
      analyseBody(StaticEnv(FunHead(name, params), deps), body)
  }

  // just collects - v - lazy value
  private def analyseBody(se: StaticEnv, v: Term): List[Call] = v match {
    case TVar(_) =>
      Nil
    case TApp(name, args) =>
      val callMatrix = buildCallMat(se.dependencies, se.caller.params, args)
      val call = Call(se.caller.name, name, callMatrix)
      call :: args.flatMap(analyseBody(se, _))
    case TCtr(c, args) =>
      args.flatMap(analyseBody(se, _))
  }

  private def buildCallMat(deps: Dependencies, callerParams: List[Ident], calleeArgs: List[Term]): CallMatrix =
    calleeArgs.map {v => callerParams.map(getRelation(deps, v, _: Ident))}

}

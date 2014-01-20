package foetus

import body._

// TODO: there is a subtlety with flatMap (see aux.scala)
// possibly, we need to use flatMap (from aux.scala) everywhere
object heart {
  // polymorphic matrices
  trait MatrixElement[E] {
    val zero: E
    val one: E
    def add(e1: E, e2: E): E
    def mult(e1: E, e2: E): E
  }

  trait MatrixOps {
    type Row
    type Matrix
    def transpose(m: Matrix): Matrix
    def mult(m1: Matrix, m2: Matrix): Matrix
    def diag(m: Matrix): Row
  }

  case class MatrixImpl[E](val elem: MatrixElement[E]) extends MatrixOps {
    type Row = List[E]
    type Matrix = List[Row]

    def transpose(m: Matrix): Matrix = m match {
      case Nil => Nil
      case Nil :: _ => Nil
      case rows => rows.map(_.head) :: transpose(rows.map(_.tail))
    }

    private def mult0(rows: Matrix, cols: Matrix): Matrix = rows match {
      case Nil => Nil
      case _ => rows.map { row => cols.map { col => (row, col).zipped.map(elem.mult).foldLeft(elem.zero)(elem.add) } }
    }

    def mult(m1: Matrix, m2: Matrix): Matrix =
      mult0(m1, transpose(m2))

    def diag(m: Matrix) = m match {
      case (el :: _) :: rows => el :: diag(rows.map(_.tail))
      case _ => Nil
    }
  }

  type Ident = Int
  val emptyIdent: Ident = 0

  type ExtIdent = (String, Ident)
  val emptyExtIdent: ExtIdent = ("", emptyIdent)

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

  // D is the type of dependencies
  trait DependenciesOps {
    type Deps
    val empty: Deps
    def addLess(deps: Deps, id1: Ident, id2: Ident): Deps
    def addEqual(deps: Deps, id1: Ident, id2: Ident): Deps
    def getRel(deps: Deps, id1: Ident, id2: Ident): Relation
  }

  object DependenciesImpl extends DependenciesOps {
    type Dep = (Ident, List[Ident])
    type Deps = List[Dep]
    override val empty = Nil

    private def findLesses(deps: Deps, x1: Ident): List[Ident] = deps.find(_._1 == x1) match {
      case Some((_, l)) => l
      case None => Nil
    }

    def addLess(deps: Deps, x1: Ident, x: Ident): Deps =
      (x1, x :: findLesses(deps, x)) :: deps

    def addEqual(deps: Deps, x1: Ident, x: Ident) =
      sys.error("not supported")

    def getRel(deps: Deps, y: Ident, x: Ident): Relation =
      if (x == y)
        RelEqual
      else
        deps.find(_._1 == y) match {
          case Some((_, l)) =>
            if (l.contains(x)) RelLess else RelUnknown
          case None =>
            RelUnknown
        }
  }

  object CallMatEl extends MatrixElement[Relation] {
    override val zero = RelUnknown
    override val one = RelEqual
    override def add(r1: Relation, r2: Relation): Relation = (r1, r2) match {
      case (RelLess, _) => RelLess
      case (_, RelLess) => RelLess
      case (RelEqual, _) => RelEqual
      case (RelUnknown, RelEqual) => RelEqual
      case (RelUnknown, RelUnknown) => RelUnknown
    }
    override def mult(r1: Relation, r2: Relation): Relation = (r1, r2) match {
      case (RelUnknown, _) => RelUnknown
      case (_, RelUnknown) => RelUnknown
      case (RelLess, _) => RelLess
      case (_, RelLess) => RelLess
      case (RelEqual, RelEqual) => RelEqual
    }
  }

  val CallMat = MatrixImpl(CallMatEl)
  type CallMatrix = CallMat.Matrix
  import CallMat._

  // Static Environment
  type FunHead = (ExtIdent, List[Ident])
  type FunStack = List[FunHead]
  case class StaticEnv(stack: FunStack, dependencies: DependenciesImpl.Deps, caller: FunHead)

  type Call = (FunHead, Ident, CallMatrix)

  // Call Graph
  trait CallGraphOps {
    type CallGraph
    val empty: CallGraph
    def add(g1: CallGraph, call: Call): CallGraph
    def addList(g: CallGraph, gs: List[Call]): CallGraph
    def getExtIdent(g: CallGraph, id: Ident): ExtIdent
    def getExtCallers(g: CallGraph): List[ExtIdent]
    def getCallers(g: CallGraph): List[Ident]
    def getCalled(g: CallGraph, id: Ident): List[Ident]
    def getCallMatrices(g: CallGraph, id1: Ident, id2: Ident): List[CallMatrix]
  }

  trait Calls extends CallGraphOps {
    type CallGraph = List[(ExtIdent, Ident, CallMatrix)]

    override val empty = Nil
    def add(calls: CallGraph, call: Call): CallGraph = call match {
      case ((f, _), g, m) => (f, g, m) :: calls
    }
    //private def add0(call: Call, calls: CallGraph) = add(calls, call)
    override def addList(calls: CallGraph, callList: List[Call]): CallGraph =
      callList.foldLeft(calls)(add)
    override def getExtIdent(g: CallGraph, id: Ident): ExtIdent = g match {
      case (f @ (name, i), _, _) :: calls =>
        if (i == id) f else getExtIdent(calls, id)
      case Nil =>
        ("UNKNOWN", id)
    }
    override def getExtCallers(g: CallGraph): List[ExtIdent] = g match {
      case Nil =>
        Nil
      case (f, _, _) :: calls =>
        val fs = getExtCallers(calls)
        if (fs.contains(f)) fs else f :: fs
    }
    override def getCallers(g: CallGraph): List[Ident] = g match {
      case Nil =>
        Nil
      case ((_, f), _, _) :: calls =>
        val fs = getCallers(calls)
        if (fs.contains(f)) fs else f :: fs
    }
    override def getCalled(g: CallGraph, id: Ident): List[Ident] = g match {
      case Nil =>
        Nil
      case ((_, f1), g, _) :: calls =>
        val gs = getCalled(calls, id)
        if (id != f1 || gs.contains(f1)) gs else g :: gs
    }
    def getCallMatrices(cg: CallGraph, f: Ident, g: Ident): List[CallMatrix] = cg match {
      case Nil =>
        Nil
      case ((_, f1), g1, m) :: calls =>
        val ms = getCallMatrices(calls, f, g)
        if (f1 == f && g1 == g) m :: ms else ms
    }
  }

  // TODO: use everywhere
  type Name = String

  def extractFunHead(pars: List[Ident], fname: ExtIdent, v: Val): (FunHead, Val) = v match {
    case VLam(x, Clos(t, env)) =>
      val i = newGen()
      extractFunHead(i :: pars, fname, hnf(Clos(t, EGen(x, i) :: env)))
    case v =>
      ((fname, pars.reverse), v)
  }

  def bareFunctionEntry(f: ExtIdent, pars: List[Ident]): Call =
    ((f, Nil), emptyIdent, Nil)

  def analyseLet(se: StaticEnv, env: Env, defs: Defs, tm: Term): List[Call] =
    analyseLet0(se, env, defs, Some(tm))

  def analyseLet0(se: StaticEnv, env: Env, defs: Defs, tm: Option[Term]): List[Call] = {
    val extDefs: List[(ExtIdent, Term)] = defs.map { case (x, t) => ((x, newGen()), t) }
    val extEnv: Env = extDefs.foldLeft(env) { (env1, extDef) =>
      val extIdent = extDef._1
      EGen.tupled(extIdent) :: env1
    }
        
    val vs: List[(ExtIdent, Val)] = extDefs.map {case (x, t) => (x, hnf(Clos(t, extEnv)))}
    val funs: List[(FunHead, Val)] = vs.map {case (x, i) => extractFunHead(Nil, x, i)}
    val fstack1 = funs.foldLeft(se.stack){(res, x) => x._1 :: res}
    val part2: List[Call] = tm match {
      case None => 
        Nil
      case Some(t) => 
        analyseBody(StaticEnv(fstack1, se.dependencies, se.caller), hnf(Clos(t, extEnv)))
    }
    val part1 = funs.flatMap{case (caller, v) => 
      bareFunctionEntry(caller._1, caller._2) :: analyseBody(StaticEnv(fstack1, se.dependencies, caller), v)}
    part1 ++ part2
  }

  // just collects
  def analyseBody(se: StaticEnv, v: Val): List[Call] = v match {
    case VCtr(c, v) => analyseBody(se, v)
    case VCtrLazy(c, cl) => analyseBody(se, hnf(cl))
    case VTup(fields) => fields.flatMap { case (_, v) => analyseBody(se, v) }
    case VTupLazy(cts, env) => cts.flatMap { case (_, v) => analyseBody(se, hnf(Clos(v, env))) }
    case VDot(v, _) => analyseBody(se, v)
    case VApp(v, clos) => analyseApp(se, v, List(clos))
    case VCase(v, pats, env) => analyseCases(se, v, pats, env)
    case VGen(x, id) => analyseCall(se, id, Nil)
    case VLet(defs, tm, env) => analyseLet(se, env, defs, tm)
    case VDef(clos) => Nil
    case VLam(x, clos) => analyseBody(se, hnf(clos))
  }

  def analyseCases0(f: (StaticEnv, Val) => List[Call], se: StaticEnv, v: Val, pats: List[Pat], env: Env): List[Call] = v match {
    case VGen(x, id) =>
      pats.flatMap(analyseCase0(f, se, env, id, _))
    case v =>
      aux.flatMap0(analyseBody(se, v), analyseCaseNoDep0(f, se, env, _: Pat), pats)
  }

  def analyseCase0(f: (StaticEnv, Val) => List[Call], se: StaticEnv, env: Env, id: Ident, pat: Pat): List[Call] = {
    val (c, (x, t)) = pat
    val id1 = newGen
    f(StaticEnv(se.stack, DependenciesImpl.addLess(se.dependencies, id1, id), se.caller), hnf(Clos(t, EGen(x, id1) :: env)))
  }

  def analyseCaseNoDep0(f: (StaticEnv, Val) => List[Call], se: StaticEnv, env: Env, pat: Pat): List[Call] = {
    val (c, (x, t)) = pat
    f(se, hnf(Clos(t, addGen(x, env))))
  }

  def analyseCases(se: StaticEnv, v: Val, pats: List[Pat], env: Env): List[Call] =
    analyseCases0(analyseBody, se, v, pats, env)

  def analyseApp(se: StaticEnv, v: Val, args: List[Clos]): List[Call] = v match {
    case VApp(v, cl) => analyseApp(se, v, cl :: args)
    case VGen(x, i) => args.map(hnf).flatMap(analyseBody(se, _))
    case VCase(v, pats, env) => analyseCases0({ (se1, v1) => analyseApp(se1, v1, args) }, se, v, pats, env)
    case VLam(x, clos) =>
      val (cl :: args1) = args
      analyseApp(se, hnf(Clos(clos.t, EVal(x, cl) :: clos.env)), args1)
    case _ => error(s"analyseApp: ${v}")
  }

  def analyseCall(se: StaticEnv, g: Ident, args: List[Clos]): List[Call] = {
    val vs = args.map(hnf)
    aux.flatMap0(List((se.caller, g, buildCallMat(se.caller._2, se.dependencies, vs))), analyseBody(se, _: Val), vs)
  }

  def buildCallMat(pars: List[Ident], deps: DependenciesImpl.Deps, vals: List[Val]): CallMatrix = vals match {
    case Nil => Nil
    case v :: vs => pars.map(getRelation(deps, v, _: Ident)) :: buildCallMat(pars, deps, vs)
  }

  def getRelation(deps: DependenciesImpl.Deps, v: Val, par: Ident): Relation = v match {
    case VGen(x, i) => DependenciesImpl.getRel(deps, i, par)
    case VDot(v, c) => getRelation(deps, v, par)
    case VApp(v, cl) => getRelation(deps, v, par)
    case _ => RelUnknown
  }

  def analyseDefs(se: StaticEnv, env: Env, defs: Defs) =
    analyseLet0(se, env, defs, None)

  val emptyStaticEnv: StaticEnv =
    StaticEnv(Nil, DependenciesImpl.empty, (emptyExtIdent, Nil))

}

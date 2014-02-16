package foetus

import aux._
import heart._
import body._

object brain {

  type CallGraph = List[Call]
  val emptyCallGraph: CallGraph = Nil

  object Elem {
    val zero: Relation = RelUnknown
    val one: Relation = RelEqual
    def add(r1: Relation, r2: Relation): Relation = (r1, r2) match {
      case (RelLess, _) => RelLess
      case (_, RelLess) => RelLess
      case (RelEqual, _) => RelEqual
      case (RelUnknown, RelEqual) => RelEqual
      case (RelUnknown, RelUnknown) => RelUnknown
    }
    def mult(r1: Relation, r2: Relation): Relation = (r1, r2) match {
      case (RelUnknown, _) => RelUnknown
      case (_, RelUnknown) => RelUnknown
      case (RelLess, _) => RelLess
      case (_, RelLess) => RelLess
      case (RelEqual, RelEqual) => RelEqual
    }
  }

  object CallMatrixOps {
    type Row = List[Relation]

    def transpose(m: CallMatrix): CallMatrix = m match {
      case Nil => Nil
      case Nil :: _ => Nil
      case rows => rows.map(_.head) :: transpose(rows.map(_.tail))
    }

    private def mult0(rows: CallMatrix, cols: CallMatrix): CallMatrix =
      rows.map { row => cols.map { col => (row, col).zipped.map(Elem.mult).foldLeft(Elem.zero)(Elem.add) } }

    def mult(m1: CallMatrix, m2: CallMatrix): CallMatrix = {
      mult0(m1, transpose(m2))
    }

    def diag(m: CallMatrix): Row =
      m.indices.map({i => m(i)(i)}).toList
  }

  case class NoOrder() extends Exception

  def dropNth[A](i: Int, xs: List[A]): List[A] = {
    val (xs1, xs2) = xs.splitAt(i)
    xs1 ++ xs2.tail
  }

  def composeCalls0(h_fs: CallGraph, g_h: Call): CallGraph = {
    val Call(caller1, callee1, callMat1) = g_h

    h_fs.foldLeft(List[Call]()) { (doneList, x) =>
      val Call(caller2, callee2, callMat2) = x
      if (caller2.id == callee1.id)
        Call(caller1, callee2, CallMatrixOps.mult(callMat2, callMat1)) :: doneList
      else
        doneList
    }
  }

  def composeCalls(h_fs: CallGraph, g_hs: CallGraph): CallGraph =
    g_hs.flatMap(composeCalls0(h_fs, _: Call))

  def complete(calls: CallGraph): CallGraph = {
    def complete0(total: List[Call]): List[Call] = {
      val composed = composeCalls(total, calls)
      val totalNew = (total ++ composed).distinct
      if (totalNew.size == total.size)
        totalNew
      else
        complete0(totalNew)
    }
    complete0(calls)
  }

  def circle(graph: CallGraph, f: Ident): List[CallMatrix] = {
    val comp = complete(graph)
    comp.foldLeft(List[CallMatrix]()) { (doneList, call) =>
      if (call.caller.id == f && call.caller.id == call.callee.id)
        call.matr :: doneList
      else
        doneList
    }
  }

  // lexical order
  def elimCols(rels: List[Relation], colNo: Int, relMat: List[List[Relation]], foundLess: Boolean): List[List[Relation]] = rels match {
    case Nil => if (foundLess) relMat else throw NoOrder()
    case RelUnknown :: _ => throw NoOrder()
    case RelLess :: rels => elimCols(rels, colNo, relMat.map(dropNth(colNo, _)), true)
    case RelEqual :: rels => elimCols(rels, colNo + 1, relMat, foundLess)
  }

  def findOrder(rowIndex: Int, relMat: List[List[Relation]], argNames: List[Int]): List[Int] = {
    log(s"relMat: $relMat")
    relMat match {
      case Nil =>
        Nil
      case Nil :: _ =>
        Nil
      case relMat =>
        try {
          try {
            val rels = relMat.apply(rowIndex)
            val rowNo = argNames(rowIndex)
            val dropped = dropNth(rowIndex, relMat)
            val tail = findOrder(0, elimCols(rels, 0, dropped, false), dropNth(rowIndex, argNames))
            rowNo :: tail
          } catch {
            case NoOrder() => findOrder(rowIndex + 1, relMat, argNames)
          }
        } catch {
          case x: IndexOutOfBoundsException =>
            throw NoOrder()
        }
    } }

  def lexicalOrder0(argRels: CallMatrix): Option[List[Int]] = argRels match {
    case Nil =>
      Some(Nil)
    case argRels =>
      try {
        Some(findOrder(0, argRels, argRels.indices.toList))
      } catch {
        case NoOrder() => None
      }
  }

  def lexicalOrder(recCalls: CallMatrix): Option[List[Int]] =
    lexicalOrder0(CallMatrixOps.transpose(recCalls))

  def checkDefs(env: Env, defs: Defs): Map[String, Option[List[Int]]] = {
    val callGraph: CallGraph = analyseDefs(emptyStaticEnv, env, defs)
    val callers: List[ExtIdent] = callGraph.map(_.caller)
    log(s"callers: $callers")
    log(s"callGraph: ${callGraph}")
    var result = Map[String, Option[List[Int]]]()
    callers.map({ f =>
      val matrices: List[CallMatrix] = circle(callGraph, f.id)
      val recMatrix: CallMatrix = matrices.map(CallMatrixOps.diag)
      val order =lexicalOrder(recMatrix)

      val nameVar = f.name
      order match {
        case None =>
          log(s"${nameVar} FAILS termination check")
        case Some(Nil) =>
          log(s"${nameVar} PASSES termination check")
        case Some(order) =>
          log(s"${nameVar} PASSES termination check (by lexical order ${order.mkString(", ")})")
      }
      f.name -> order
    }).toMap
  }
}

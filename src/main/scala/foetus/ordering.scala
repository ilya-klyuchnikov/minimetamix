package foetus

import common.ast._
import foetus.calls._

object ordering {

  implicit class RelationOps(r1: Relation) {
    def *(r2: Relation): Relation = (r1, r2) match {
      case (`?`, _) | (_, `?`) => `?`
      case (`<`, _) | (_, `<`) => `<`
      case (`=`, `=`)          => `=`
    }
    def +(r2: Relation): Relation = (r1, r2) match {
      case (`<`, _) | (_, `<`) => `<`
      case (`=`, _) | (_, `=`) => `=`
      case (`?`, `?`)          => `?`
    }
  }

  implicit class CallMatrixOps(m1: CallMatrix) {
    def *(m2: CallMatrix): CallMatrix =
      for { row <- m1 } yield for { col <- m2.transpose } yield (row, col).zipped
        .map(_ * _)
        .reduce(_ + _)

    def diag: List[Relation] =
      for (i <- m1.indices.toList) yield m1(i)(i)
  }

  /** @param defs
    *   - definitions to order
    * @return
    *   a list of pairs (f -> Option[Order])
    */
  def orderDefs(defs: List[Def]): List[(String, Option[List[Int]])] = {
    val graph: CallGraph = callGraph(defs)
    val callers: List[String] = defs.map(_.name)
    val completeGraph = saturate(graph)
    callers.map { f =>
      val matrices: List[CallMatrix] = circles(completeGraph, f)
      val paramOrders: List[List[Relation]] = matrices.map(_.diag)
      val order = lexical.lexicalOrder(paramOrders)
      f -> order
    }
  }

  // transitive closure of a graph
  private def saturate(calls: CallGraph): CallGraph = {
    var total, total1 = calls
    do {
      total = total1
      total1 = (total ++ compose(total, calls)).distinct
    } while (total != total1)
    total1
  }

  private def compose(cg1: CallGraph, cg2: CallGraph): CallGraph =
    for {
      Call(caller1, callee1, callMat1) <- cg1
      Call(caller2, callee2, callMat2) <- cg2 if callee1 == caller2
    } yield Call(caller1, callee2, callMat2 * callMat1)

  private def circles(graph: CallGraph, f: String): List[CallMatrix] =
    for (Call(`f`, `f`, m) <- graph) yield m
}

package foetus

import aux._
import heart._
import body._

// check.sml
object brain extends Calls {

  case class NoOrder() extends Exception

  private def compose0(hs: List[CallMatrix], g: CallMatrix): List[CallMatrix] =
    hs.map(CallMat.mult(_, g))

  def compose(hs: List[CallMatrix], gs: List[CallMatrix]): List[CallMatrix] =
    flatMap(compose0(hs, _: CallMatrix), gs)

  def composeCallsV0[A, B, C, D, E](
    h_fs: List[(((A, B), C, CallMatrix), List[D])],
    g_h: ((E, B, CallMatrix), List[D])): List[((E, C, CallMatrix), List[D])] = {
    val ((caller1, callee1, callMat1), visited1) = g_h

    h_fs.foldLeft(Nil: List[((E, C, CallMatrix), List[D])]) { (doneList, a) =>
      val (((_, caller2), callee2, callMat2), visited2) = a
      if (caller2 == callee1)
        ((caller1, callee2, CallMat.mult(callMat2, callMat1)), visited1 ++ visited2) :: doneList
      else doneList
    }
  }

  def composeCallsV[A, B, C, D, E](
    h_fs: List[(((A, B), C, CallMatrix), List[D])],
    g_hs: List[((E, B, CallMatrix), List[D])]): List[((E, C, CallMatrix), List[D])] = {
    flatMap({ g_h: ((E, B, CallMatrix), List[D]) => composeCallsV0(h_fs, g_h) }, g_hs)
  }

  def unionV[A, B](set1: List[(A, B)], set2: List[(A, B)]): List[(A, B)] = set2 match {
    case Nil =>
      set1
    case (el, v) :: set2 =>
      if (set1.exists(_._1 == el)) unionV(set1, set2) else unionV((el, v) :: set1, set2)
  }

  def completeV0[D](total: List[(Call2, List[D])], basic: List[(Call2, List[D])]): List[(Call2, List[D])] = {
    val composedCalls: List[(Call2, List[D])] =
      composeCallsV(total, basic)
    val totalNew: List[(Call2, List[D])] =
      unionV(total, composedCalls)
    if (totalNew.size == total.size)
      totalNew
    else
      completeV0(totalNew, basic)
  }
  /*
  def completeV[A](calls: List[(Call2, List[A])]): List[(Call2, List[A])] = {
    completeV0(calls, calls)
  }
  */

  def completeV[A, B, C](calls: List[(((A, B), B, CallMatrix), List[C])]): List[(((A, B), B, CallMatrix), List[C])] = {
    def completeV0(total: List[(((A, B), B, CallMatrix), List[C])]): List[(((A, B), B, CallMatrix), List[C])] = {
      val composedCalls =
        composeCallsV(total, calls)
      val totalNew =
        unionV(total, composedCalls)
      if (totalNew.size == total.size)
        totalNew
      else
        completeV0(totalNew)
    }
    completeV0(calls)
  }

  def createVisitedList[A, B, C](call: (A, B, C)): ((A, B, C), List[B]) =
    (call, List(call._2))

  // V for verbose - ha-ha
  def circleV(calls: CallGraph, f: Ident): List[CallMatrix] = {
    def extendId(i: Ident) = { (j: Ident) => extIdentToString(getExtIdent(calls, i), j) }
    val visited = calls.map(createVisitedList)
    val comp = completeV(visited)
    // TODO: debug here (maybe, it is debugged in original)
    comp.foldLeft(List[CallMatrix]()) { (doneList, call) =>
      val (((_, caller), callee, callMat), visited) = call
      if (caller == f && caller == callee) callMat :: doneList else doneList
    }
  }

  def composeCalls0[A, B, C, D](
    h_fs: List[((A, B), C, CallMatrix)],
    g_h: (D, B, CallMatrix)): List[(D, C, CallMatrix)] = {
    val (caller1, callee1, callMat1) = g_h
    h_fs.foldLeft(List[(D, C, CallMatrix)]()) { (doneList, x) =>
      val ((_, caller2), callee2, callMat2) = x
      if (caller2 == callee1)
        (caller1, callee2, CallMat.mult(callMat2, callMat1)) :: doneList
      else
        doneList
    }
  }

  def composeCalls[A, B, C, D](h_fs: List[((A, B), C, CallMatrix)], g_hs: List[(D, B, CallMatrix)]): List[(D, C, CallMatrix)] =
    flatMap(composeCalls0(h_fs, _: (D, B, CallMatrix)), g_hs)

  def complete[A, B](calls: List[((A, B), B, CallMatrix)]): List[((A, B), B, CallMatrix)] = {
    def complete0(total: List[((A, B), B, CallMatrix)]): List[((A, B), B, CallMatrix)] = {
      val composed = composeCalls(total, calls)
      val totalNew = union(total, composed)
      if (totalNew.size == total.size) totalNew else complete0(totalNew)
    }
    complete0(calls)
  }

  def circle(calls: CallGraph, f: Ident): List[CallMatrix] = {
    val comp = complete(calls)
    log(s"comp: $comp")
    comp.foldLeft(List[CallMatrix]()) { (doneList, call) =>
      log(s"call: $call")
      val ((_, caller), callee, callMat) = call
      if (caller == f && caller == callee) callMat :: doneList else doneList
    }
  }

  // lexical order
  
  def elimCols(rels: List[Relation], colNo: Int, relMat: List[List[Relation]], foundLess: Boolean): List[List[Relation]] = rels match {
    case Nil => if (foundLess) relMat else throw NoOrder()
    case RelUnknown :: _ => throw NoOrder()
    case RelLess :: rels => elimCols(rels, colNo, relMat.map(dropNth(colNo, _)), true)
    case RelEqual :: rels => elimCols(rels, colNo + 1, relMat, foundLess)
  }
  
  def findOrder[A](rowIndex: Int, relMat: List[List[Relation]], argNames: List[A]): List[A] = {
    log(s"relMat: $relMat")
    relMat match {
    case Nil =>
      Nil
    case Nil :: _ =>
      Nil
    case relMat =>
      try {
          val rels = relMat.apply(rowIndex)
          val rowNo = argNames(rowIndex)
          val tail = try {
            val dropped = dropNth(rowIndex, relMat)
            log(s"dropped: $dropped")
            log(s"rels: $rels")
            findOrder(0, elimCols(rels, 0, dropped, false), dropNth(rowIndex, argNames))
          } catch {
            case NoOrder() => findOrder(rowIndex + 1, relMat, argNames)
          }
          rowNo :: tail
      } catch {
        case x: IndexOutOfBoundsException =>
          throw NoOrder()
      }
  } }

  // CallMatrix = List[List[Relation]]
  def lexicalOrder0(argRels: CallMatrix): Option[List[Int]] = {
    log(s"CallMatrix: $argRels")
    argRels match {
    case Nil =>
      Some(Nil)
    case argRels =>
      try {
        Some(findOrder(0, argRels, upto(0, argRels.size)))
      } catch {
        case NoOrder() => None
      }
  }}

  def lexicalOrder(recCalls: CallMatrix): Option[List[Int]] =
    lexicalOrder0(CallMat.transpose(recCalls))

  def checkDefs(env: Env, defs: Defs): Boolean = {
    println("======================")
    println(s"checking ${defs.map(_._1).mkString(", ")}")

    val calls: CallGraph = union(Nil, addList(empty, analyseDefs(emptyStaticEnv, env, defs)))
    log(s"calls: ${calls}")
    def term(f: Int): Option[List[Int]] = {
      val c = circle(calls, f)
      log(s"circle: ${c}")
      val xxx = c.map(CallMat.diag)
      log(s"xxx: $xxx")
      lexicalOrder(xxx)
    }
    val extCallers: List[ExtIdent] = getExtCallers(calls).reverse
    log(s"extCallers: $extCallers")
    var result = true
    extCallers.foldLeft(List[String]()) { (variants, pair) =>
      val (name, f) = pair
      val nameVar = variant(name, variants)
      term(f) match {
        case None =>
          result = false
          println(s"${nameVar} FAILS termination check")
        case Some(Nil) =>
          println(s"${nameVar} PASSES termination check")
        case Some(order) =>
          println(s"${nameVar} PASSES termination check (by lexical order ${order.mkString(", ")})")
      }
      nameVar :: variants
    }
    result
  }

}
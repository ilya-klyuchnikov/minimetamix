package sll.test.nameless

import common.parser.parseDefs
import sll.data._
import sll.translator._

/** The quoted version of Nameless0 */
object Nameless0Q {

  def getF0(n: String): DExp0 = ???
  def getG00(n: String, pn: String) : DExp0 = ???

  sealed trait Val
  case class Ctr0(name: String) extends Val
  case class Ctr1(name: String, arg1: Val) extends Val
  case class Ctr2(name: String, arg1: Val, arg2: Val) extends Val
  case class Err() extends Val

  sealed trait DExp
  sealed trait DExp0 extends DExp

  case class DCtr0(name: String) extends DExp0
  case class DCtr10(name: String, arg1: DExp0) extends DExp0
  case class DCtr200(name: String, arg1: DExp0, arg2: DExp0) extends DExp0
  case class DFCall0(name: String) extends DExp0

  sealed trait IExp
  case class IVal(v: Val) extends IExp
  case class IFCall0(name: String) extends IExp
  case class IGCall1(name: String, arg1: Val) extends IExp

  def blaze[A](a: A): A = a

  val program: Program = parseDefs {
    def iEval(iExp: IExp): Val = iExp match {
      case IVal(v) => v
      case IFCall0(n) => dEval0(blaze(getF0(n)))
      case IGCall1(n, farg1) => switchVal(farg1, n)
    }

    def switchVal(v: Val, gn: String): Val = v match {
      case Ctr0(pn) => dEval0(blaze(getG00(gn, pn)))
      case Ctr1(pn, arg1) => Err()
      case Ctr2(pn, arg1, arg2) => Err()
      case Err() => Err()
    }

    def dEval0(exp: DExp0): Val = exp match {
      case DCtr0(n) => Ctr0(n)
      case DCtr10(n, arg) => Ctr1(n, dEval0(arg))
      case DCtr200(n, arg1, arg2) => Ctr2(n, dEval0(arg1), dEval0(arg2))
      case DFCall0(n) => dEval0(blaze(getF0(n)))
    }
  }
}

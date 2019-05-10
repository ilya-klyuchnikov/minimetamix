package sll.test.nameless

import common.parser.parseDefs
import sll.data._
import sll.translator._

/** The quoted version of Nameless2 */
object Nameless2Q {

  def getF0(n: String): DExp0 = ???
  def getF1(n: String): DExp1 = ???
  def getF2(n: String): DExp2 = ???
  def getG00(n: String, pn: String): DExp0 = ???
  def getG01(n: String, pn: String): DExp1 = ???
  def getG10(n: String, pn: String): DExp1 = ???
  def getG11(n: String, pn: String): DExp2 = ???
  def getG20(n: String, pn: String): DExp2 = ???

  sealed trait Val
  case class Ctr0(name: String) extends Val
  case class Ctr1(name: String, arg1: Val) extends Val
  case class Ctr2(name: String, arg1: Val, arg2: Val) extends Val
  case class Err() extends Val

  sealed trait DExp
  sealed trait DExp0 extends DExp
  sealed trait DExp1 extends DExp
  sealed trait DExp2 extends DExp

  case class DCtr0(name: String) extends DExp0
  case class DCtr10(name: String, arg1: DExp0) extends DExp0
  case class DCtr200(name: String, arg1: DExp0, arg2: DExp0) extends DExp0
  case class DFCall0(name: String) extends DExp0

  case class DVar() extends DExp1
  case class DCtr11(name: String, arg1: DExp1) extends DExp1
  case class DCtr210(name: String, arg1: DExp1, arg2: DExp0) extends DExp1
  case class DCtr201(name: String, arg1: DExp0, arg2: DExp1) extends DExp1
  case class DFCall1(name: String, arg1: DVar) extends DExp1
  case class DGCall1(name: String, arg1: DVar) extends DExp1

  case class DCtr12(name: String, arg1: DExp2) extends DExp2
  case class DCtr211(name: String, arg1: DExp1, arg2: DExp1) extends DExp2
  case class DFCall2(name: String, arg1: DVar, arg2: DVar) extends DExp2
  case class DGCall2(name: String, arg1: DVar, arg2: DVar) extends DExp2

  sealed trait IExp
  case class IVal(v: Val) extends IExp
  case class IFCall0(name: String) extends IExp
  case class IFCall1(name: String, arg1: Val) extends IExp
  case class IGCall1(name: String, arg1: Val) extends IExp
  case class IFCall2(name: String, arg1: Val, arg2: Val) extends IExp
  case class IGCall2(name: String, arg1: Val, arg2: Val) extends IExp

  def blaze[A](a: A): A = a

  val program: Program = parseDefs {
    def iEval(iExp: IExp): Val = iExp match {
      case IVal(v) => v
      case IFCall0(n) => dEval0(blaze(getF0(n)))
      case IFCall1(n, farg1) => dEval1(blaze(getF1(n)), farg1)
      case IGCall1(n, farg1) => switchVal(farg1, n)
      case IFCall2(n, farg1, farg2) => dEval2(blaze(getF2(n)), farg1, farg2)
      case IGCall2(n, farg1, farg2) => switchVal1(farg1, n, farg2)
    }

    def switchVal(v: Val, gn: String): Val = v match {
      case Ctr0(pn) => dEval0(blaze(getG00(gn, pn)))
      case Ctr1(pn, arg1) => dEval1(blaze(getG10(gn, pn)), arg1)
      case Ctr2(pn, arg1, arg2) => dEval2(blaze(getG20(gn, pn)), arg1, arg2)
      case Err() => Err()
    }

    def switchVal1(v: Val, gn: String, arg2: Val): Val = v match {
      case Ctr0(pn) => dEval1(blaze(getG01(gn, pn)), arg2)
      case Ctr1(pn, arg1) => dEval2(blaze(getG11(gn, pn)), arg1, arg2)
      case Ctr2(pn, carg1, carg2) => Err()
      case Err() => Err()
    }

    def dEval0(exp: DExp0): Val = exp match {
      case DCtr0(n) => Ctr0(n)
      case DCtr10(n, arg) => Ctr1(n, dEval0(arg))
      case DCtr200(n, arg1, arg2) => Ctr2(n, dEval0(arg1), dEval0(arg2))
      case DFCall0(n) => dEval0(blaze(getF0(n)))
    }

    def dEval1(exp: DExp1, arg1: Val): Val = exp match {
      case DVar() => arg1
      case DFCall1(n, farg1) => dEvalF1(farg1, n, arg1)
      case DGCall1(n, farg1) => dEvalG1(farg1, n, arg1)
      case DCtr11(n, carg1) => Ctr1(n, dEval1(carg1, arg1))
      case DCtr210(n, carg1, carg2) => Ctr2(n, dEval1(carg1, arg1), dEval0(carg2))
      case DCtr201(n, carg1, carg2) => Ctr2(n, dEval0(carg1), dEval1(carg2, arg1))
    }

    def dEvalF1(farg1: DVar, n: String, arg1: Val): Val = farg1 match {
      case DVar() => dEval1(blaze(getF1(n)), arg1)
    }

    def dEvalG1(farg1: DVar, n: String, arg1: Val): Val = farg1 match {
      case DVar() => switchVal(arg1, n)
    }

    def dEval2(exp: DExp2, arg1: Val, arg2: Val): Val = exp match {
      case DFCall2(n, farg1, farg2) => dEvalF21(farg1, farg2, n, arg1, arg2)
      case DGCall2(n, farg1, farg2) => dEvalG21(farg1, farg2, n, arg1, arg2)
      case DCtr12(n, carg1) => Ctr1(n, dEval2(carg1, arg1, arg2))
      case DCtr211(n, carg1, carg2) => Ctr2(n, dEval1(carg1, arg1), dEval1(carg2, arg2))
    }

    def dEvalF21(farg1: DVar, farg2: DVar, n: String, arg1: Val, arg2: Val): Val = farg1 match {
      case DVar() => dEvalF22(farg2, n, arg1, arg2)
    }

    def dEvalF22(farg2: DVar, n: String, arg1: Val, arg2: Val): Val = farg2 match {
      case DVar() => dEval2(blaze(getF2(n)), arg1, arg2)
    }

    def dEvalG21(farg1: DVar, farg2: DVar, gn: String, arg1: Val, arg2: Val): Val = farg1 match {
      case DVar() => dEvalG22(farg2, gn, arg1, arg2)
    }

    def dEvalG22(farg2: DVar, gn: String, arg1: Val, arg2: Val): Val = farg2 match {
      case DVar() => switchVal1(arg1, gn, arg2)
    }
  }
}

package sll.test.nameless

/** A specialization of the SLL interpreter wrt Nameless1AST */
trait Nameless1 {
  import Nameless1AST._

  def getF0(n: String): DExp0
  def getF1(n: String): DExp1
  def getG00(n: String, pn: String): DExp0
  def getG01(n: String, pn: String): DExp1
  def getG10(n: String, pn: String): DExp1

  def blaze[A](a: A): A = a

  def iEval(iExp: IExp): Val = iExp match {
    case IVal(v) => v
    case IFCall0(n) => dEval0(blaze(getF0(n)))
    case IFCall1(n, farg1) => dEval1(blaze(getF1(n)), farg1)
    case IGCall1(n, farg1) => switchVal(farg1, n)
  }

  def switchVal(v: Val, gn: String): Val = v match {
    case Ctr0(pn) => dEval0(blaze(getG00(gn, pn)))
    case Ctr1(pn, arg1) => dEval1(blaze(getG10(gn, pn)), arg1)
    case Ctr2(pn, arg1, arg2) => Err()
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
    case DFCall1(n) => dEval1(blaze(getF1(n)), arg1)
    case DGCall1(n) => switchVal(arg1, n)
    case DCtr11(n, carg1) => Ctr1(n, dEval1(carg1, arg1))
    case DCtr210(n, carg1, carg2) => Ctr2(n, dEval1(carg1, arg1), dEval0(carg2))
    case DCtr201(n, carg1, carg2) => Ctr2(n, dEval0(carg1), dEval1(carg2, arg1))
  }
}

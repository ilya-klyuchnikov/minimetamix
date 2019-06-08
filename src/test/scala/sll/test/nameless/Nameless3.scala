package sll.test.nameless

/** A specialization of the SLL interpreter wrt Nameless3AST */
trait Nameless3 {
  import Nameless3AST._

  def getF0(n: String): DExp0
  def getF1(n: String): DExp1
  def getF2(n: String): DExp2
  def getF3(n: String): DExp3

  def g1_00(pn: String): DExp0
  def g1_01(pn: String): DExp1
  def g1_02(pn: String): DExp2

  def g1_10(pn: String): DExp1
  def g1_11(pn: String): DExp2
  def g1_12(pn: String): DExp3

  def g1_20(pn: String): DExp2
  def g1_21(pn: String): DExp3

  case class G1() extends GN

  def blaze[A](a: A): A = a

  def fSwitch0(fn: String): Val =
    eval0(blaze(getF0(fn)))

  def gSwitch0(gn: GN)( v: Val): Val = gn match {
    case G1() => switch0_g1(v)
  }

  def switch0_g1(v: Val): Val = v match {
    case Ctr0(pn) => eval0(blaze(g1_00(pn)))
    case Ctr1(pn, arg1) => eval1(blaze(g1_10(pn)))(arg1)
    case Ctr2(pn, arg1, arg2) => eval2(blaze(g1_20(pn)))(arg1, arg2)
  }

  def fSwitch1(fn: String)(arg1: Val): Val =
    eval1(blaze(getF1(fn)))(arg1)

  def gSwitch1(gn: GN, v: Val, arg2: Val): Val = gn match {
    case G1() => switch1_g1(v)(arg2)
  }

  def switch1_g1(v: Val)(arg1: Val): Val = v match {
    case Ctr0(pn) => eval1(blaze(g1_01(pn)))(arg1)
    case Ctr1(pn, carg1) => eval2(blaze(g1_11(pn)))(carg1, arg1)
    case Ctr2(pn, carg1, carg2) => eval3(blaze(g1_21(pn)))(carg1, carg2, arg1)
  }

  def gSwitch2(gn: GN, v: Val, arg1: Val, arg2: Val): Val = gn match {
    case G1() => switch2_g1(v)(arg1, arg2)
  }

  def switch2_g1(v: Val)(arg1: Val, arg2: Val): Val = v match {
    case Ctr0(pn) => eval2(blaze(g1_02(pn)))(arg1, arg2)
    case Ctr1(pn, carg1) => eval3(blaze(g1_12(pn)))(carg1, arg1, arg2)
  }

  def fSwitch2(fn: String)(arg1: Val, arg2: Val): Val =
    eval2(blaze(getF2(fn)))(arg1, arg2)

  def fSwitch3(fn: String)(arg1: Val, arg2: Val, arg3: Val): Val =
    eval3(blaze(getF3(fn)))(arg1, arg2, arg3)

  def eval0(exp: DExp0): Val = exp match {
    case DCtr0(n) => Ctr0(n)
    case DCtr10(n, arg) => Ctr1(n, eval0(arg))
    case DCtr200(n, arg1, arg2) => Ctr2(n, eval0(arg1), eval0(arg2))
    case DFCall0(n) => fSwitch0(n)
  }

  def eval1(exp: DExp1)(arg1: Val): Val = exp match {
    case DVar() => arg1
    case DFCall1(n) => fSwitch1(n)(arg1)
    case DGCall1(n) => gSwitch0(n)(arg1)
    case DCtr11(n, carg1) => Ctr1(n, eval1(carg1)(arg1))
    case DCtr210(n, carg1, carg2) => Ctr2(n, eval1(carg1)(arg1), eval0(carg2))
    case DCtr201(n, carg1, carg2) => Ctr2(n, eval0(carg1), eval1(carg2)(arg1))
  }

  def eval2(exp: DExp2)(arg1: Val, arg2: Val): Val = exp match {
    case DFCall2(n) => fSwitch2(n)(arg1, arg2)
    case DGCall2(n) => gSwitch1(n, arg1, arg2)
    case DCtr12(n, carg1) => Ctr1(n, eval2(carg1)(arg1, arg2))
    case DCtr211(n, carg1, carg2) => Ctr2(n, eval1(carg1)(arg1), eval1(carg2)(arg2))
  }

  def eval3(exp: DExp3)(arg1: Val, arg2: Val, arg3: Val): Val = exp match {
    case DFCall3(n) => fSwitch3(n)(arg1, arg2, arg3)
    case DCtr1_3(n, carg1) => Ctr1(n, eval3(carg1)(arg1, arg2, arg3))
    case DCtr2_03(n, carg1, carg2) => Ctr2(n, eval0(carg1), eval3(carg2)(arg1, arg2, arg3))
    case DCtr2_12(n, carg1, carg2) => Ctr2(n, eval1(carg1)(arg1), eval2(carg2)(arg2, arg3))
    case DCtr2_21(n, carg1, carg2) => Ctr2(n, eval2(carg1)(arg1, arg2), eval1(carg2)(arg3))
    case DCtr2_30(n, carg1, carg2) => Ctr2(n, eval3(carg1)(arg1, arg2, arg3), eval0(carg2))
  }

  def iEval(iExp: IExp): Val = iExp match {
    case IVal(v) => v
    case IFCall0(n) => fSwitch0(n)
    case IFCall1(n, arg1) => fSwitch1(n)(arg1)
    case IFCall2(n, arg1, arg2) => fSwitch2(n)(arg1, arg2)
    case IFCall3(n, arg1, arg2, arg3) => fSwitch3(n)(arg1, arg2, arg3)
    case IGCall1(n, arg1) => gSwitch0(n)(arg1)
    case IGCall2(n, arg1, arg2) => gSwitch1(n, arg1, arg2)
    case IGCall3(n, arg1, arg2, arg3) => gSwitch2(n, arg1, arg2, arg3)
  }
}

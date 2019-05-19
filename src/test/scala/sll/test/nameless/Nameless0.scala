package sll.test.nameless

/** A specialization of the SLL interpreter wrt Nameless0AST */
trait Nameless0 {
  import Nameless0AST._

  def getF0(n: String): DExp0
  def getG00(n: String, pn: String): DExp0

  def blaze[A](a: A): A = a

  def fSwitch0(fn: String): Val =
    eval0(blaze(getF0(fn)))

  def gSwitch0(v: Val, gn: String): Val = v match {
    case Ctr0(pn) => eval0(blaze(getG00(gn, pn)))
    case Ctr1(pn, arg1) => Err()
    case Ctr2(pn, arg1, arg2) => Err()
    case Err() => Err()
  }

  def eval0(exp: DExp0): Val = exp match {
    case DCtr0(n) => Ctr0(n)
    case DCtr10(n, arg) => Ctr1(n, eval0(arg))
    case DCtr200(n, arg1, arg2) => Ctr2(n, eval0(arg1), eval0(arg2))
    case DFCall0(n) => fSwitch0(n)
  }

  def iEval(iExp: IExp): Val = iExp match {
    case IVal(v) => v
    case IFCall0(n) => fSwitch0(n)
    case IGCall1(n, farg1) => gSwitch0(farg1, n)
  }
}

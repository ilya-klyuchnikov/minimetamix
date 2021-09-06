package sll.test.nameless

import common.parser.parseDefs
import sll.data._
import sll.translator._

/** The quoted version of Nameless0 */
object Nameless1Q {
  import Nameless1AST._

  def getF0(n: String): DExp0 = ???
  def getF1(n: String): DExp1 = ???
  def getG00(n: String, pn: String): DExp0 = ???
  def getG01(n: String, pn: String): DExp1 = ???
  def getG10(n: String, pn: String): DExp1 = ???

  def blaze[A](a: A): A = a

  val program: Program = parseDefs {
    def fSwitch0(fn: String): Val =
      eval0(blaze(getF0(fn)))

    def gSwitch0(v: Val, gn: String): Val = v match {
      case Ctr0(pn)             => eval0(blaze(getG00(gn, pn)))
      case Ctr1(pn, arg1)       => eval1(blaze(getG10(gn, pn)), arg1)
      case Ctr2(pn, arg1, arg2) => Err()
      case Err()                => Err()
    }

    def fSwitch1(fn: String, arg1: Val): Val =
      eval1(blaze(getF1(fn)), arg1)

    def eval0(exp: DExp0): Val = exp match {
      case DCtr0(n)               => Ctr0(n)
      case DCtr10(n, arg)         => Ctr1(n, eval0(arg))
      case DCtr200(n, arg1, arg2) => Ctr2(n, eval0(arg1), eval0(arg2))
      case DFCall0(n)             => fSwitch0(n)
    }

    def eval1(exp: DExp1, arg1: Val): Val = exp match {
      case DVar()                   => arg1
      case DFCall1(n)               => fSwitch1(n, arg1)
      case DGCall1(n)               => gSwitch0(arg1, n)
      case DCtr11(n, carg1)         => Ctr1(n, eval1(carg1, arg1))
      case DCtr210(n, carg1, carg2) => Ctr2(n, eval1(carg1, arg1), eval0(carg2))
      case DCtr201(n, carg1, carg2) => Ctr2(n, eval0(carg1), eval1(carg2, arg1))
    }

    def iEval(iExp: IExp): Val = iExp match {
      case IVal(v)          => v
      case IFCall0(n)       => fSwitch0(n)
      case IFCall1(n, arg1) => fSwitch1(n, arg1)
      case IGCall1(n, arg1) => gSwitch0(arg1, n)
    }
  }
}

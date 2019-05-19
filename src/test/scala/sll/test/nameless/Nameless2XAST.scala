package sll.test.nameless

/** The eXtended version of Nameless2AST: any order of variables in the body. */
object Nameless2XAST {
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
  case class DFCall1(name: String) extends DExp1
  case class DGCall1(name: String) extends DExp1

  case class DCtr12(name: String, arg1: DExp2) extends DExp2
  case class DCtr211_1(name: String, arg1: DExp1, arg2: DExp1) extends DExp2
  case class DCtr211_2(name: String, arg1: DExp1, arg2: DExp1) extends DExp2
  case class DFCall2_1(name: String) extends DExp2
  case class DFCall2_2(name: String) extends DExp2
  case class DGCall2_1(name: String) extends DExp2
  case class DGCall2_2(name: String) extends DExp2

  sealed trait IExp
  case class IVal(v: Val) extends IExp
  case class IFCall0(name: String) extends IExp
  case class IFCall1(name: String, arg1: Val) extends IExp
  case class IGCall1(name: String, arg1: Val) extends IExp
  case class IFCall2(name: String, arg1: Val, arg2: Val) extends IExp
  case class IGCall2(name: String, arg1: Val, arg2: Val) extends IExp
}

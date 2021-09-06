package sll.test.nameless

/** A specialization of the SLL syntax: the maximal number of variables in the body a definition is
  *   1. Variables in the rhs should be in the same order as in the lhs.
  */
object Nameless1AST {
  sealed trait Val
  case class Ctr0(name: String) extends Val
  case class Ctr1(name: String, arg1: Val) extends Val
  case class Ctr2(name: String, arg1: Val, arg2: Val) extends Val
  case class Err() extends Val

  sealed trait DExp
  sealed trait DExp0 extends DExp
  sealed trait DExp1 extends DExp

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

  sealed trait IExp
  case class IVal(v: Val) extends IExp
  case class IFCall0(name: String) extends IExp
  case class IFCall1(name: String, arg1: Val) extends IExp
  case class IGCall1(name: String, arg1: Val) extends IExp
}

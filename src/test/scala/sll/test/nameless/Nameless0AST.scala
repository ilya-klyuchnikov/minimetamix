package sll.test.nameless

/** A specialization of the SLL syntax: the maximal number of variables in the body a definition is
  * 0.
  */
object Nameless0AST {
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
}

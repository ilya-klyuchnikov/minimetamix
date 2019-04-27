package foetus

/**
  * A collection of algebraic data types used in the definitions.
  */
object adt {

  sealed trait Bool
  case class True() extends Bool
  case class False() extends Bool

  sealed trait Nat
  case class Z() extends Nat
  case class S(pred: Nat) extends Nat

  sealed trait List[A]
  case class Nil[A]() extends List[A]
  case class Cons[A](head: A, tail: List[A]) extends List[A]

}

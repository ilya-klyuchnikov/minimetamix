package foetus.test

import foetus.calls._
import foetus.lexical._

class LexicalSpec extends org.scalatest.FunSpec with org.scalatest.Matchers {
  val ?? = RelUnknown
  val == = RelEqual
  val << = RelLess

  describe("LexicalSpec.scala") {
    lexicalOrder(List(
      List(??)
    )) should equal {None}

    lexicalOrder(
      List(
        List(==)
      )
    ) should equal {None}

    lexicalOrder(
      List(
        List(<<)
      )
    ) should equal {Some(List(0))}

    lexicalOrder(
      List(
        List(??, <<, ==),
        List(<<, ==, <<)
      )
    ) should equal {Some(List(1, 0))}

    lexicalOrder(
      List(
        List(??, <<, ==),
        List(<<, ==, <<),
        List(??, ==, <<)
      )
    ) should equal {Some(List(1, 2))}

    lexicalOrder(
      List(
        List(??, <<, ==, ==),
        List(<<, ==, ??, ??),
        List(??, ==, <<, ??)
      )
    ) should equal {None}
  }
}

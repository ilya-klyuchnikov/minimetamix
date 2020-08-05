package foetus.test

import foetus.calls._
import foetus.lexical._

class LexicalSpec extends org.scalatest.funspec.AnyFunSpec {

  describe("LexicalSpec.scala") {
    assert(
      lexicalOrder(
        List(
          List(`?`)
        )
      ) === None)

    assert(
      lexicalOrder(
        List(
          List(`=`)
        )
      ) === None)

    assert(
      lexicalOrder(
        List(
          List(`<`)
        )
      ) === Some(List(0)))

    assert(
      lexicalOrder(
        List(
          List(`?`, `<`, `=`),
          List(`<`, `=`, `<`)
        )
      ) === Some(List(1, 0)))

    assert(
      lexicalOrder(
        List(
          List(`?`, `<`, `=`),
          List(`<`, `=`, `<`),
          List(`?`, `=`, `<`)
        )
      ) === Some(List(1, 2)))

    assert(
      lexicalOrder(
        List(
          List(`?`, `<`, `=`, `=`),
          List(`<`, `=`, `?`, `?`),
          List(`?`, `=`, `<`, `?`)
        )
      ) === None)
  }
}

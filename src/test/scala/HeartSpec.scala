package foetus.test

class HeartSpec extends org.scalatest.FunSpec {
  import foetus.body._
  import foetus.heart._
  import foetus.parser._

  describe("case1") {
    val defs = parseDefs {
      sealed trait Nat
      val id: Nat => Nat = {x => x}
    }

    val calls = analyseDefs(emptyStaticEnv, Env(), defs)
    println(calls)
  }

  describe("case2") {
    val defs = parseDefs {
      sealed trait Nat
      val id: Nat => Nat = {x => id(x)}
    }

    val calls = analyseDefs(emptyStaticEnv, Env(), defs)
    println(calls)
  }
}

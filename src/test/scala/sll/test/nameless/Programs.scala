package sll.test.nameless

object Programs {
  // add(Z(), y) = y
  // add(S(x), y) = S(add(x, y))
  object P3 extends Nameless3 {
    import Nameless3AST._

    override def getF0(n: String): DExp0 =
      sys.error(s"F0: $n")
    override def getF1(n: String): DExp1 =
      sys.error(s"F1: $n")
    override def getF2(n: String): DExp2 =
      sys.error(s"F2: $n")
    override def getF3(n: String): DExp3 =
      sys.error(s"F3: $n")
    override def getG00(n: String, pn: String): DExp0 =
      sys.error(s"G00: $n, $pn")
    override def getG01(n: String, pn: String): DExp1 = (n, pn) match {
      case ("add", "Z") => DVar()
      case _            => sys.error(s"getG01: $n, $pn")
    }
    override def getG10(n: String, pn: String): DExp1 =
      sys.error(s"getG10: $n, $pn")
    override def getG11(n: String, pn: String): DExp2 = (n, pn) match {
      // add(S(x), y) = S(add(x, y))
      case ("add", "S") => DCtr12("S", DGCall2("add"))
      case _            => sys.error(s"getG01: $n, $pn")
    }
    override def getG20(n: String, pn: String): DExp2 =
      sys.error(s"getG20: $n, $pn")
    override def getG21(n: String, pn: String): DExp3 =
      sys.error(s"getG21: $n, $pn")
  }

  // flip(L(x)) = L(x)
  // flip(B(x, y)) = B(flip(y), flip(x))
  object P2 extends Nameless2X {
    import Nameless2XAST._

    override def getF0(n: String): DExp0 =
      sys.error(s"F0: $n")
    override def getF1(n: String): DExp1 =
      sys.error(s"F1: $n")
    override def getF2(n: String): DExp2 =
      sys.error(s"F2: $n")
    override def getG00(n: String, pn: String): DExp0 =
      sys.error(s"G00: $n, $pn")
    override def getG01(n: String, pn: String): DExp1 =
      sys.error(s"G01: $n, $pn")
    override def getG10(n: String, pn: String): DExp1 = (n, pn) match {
      case ("flip", "L") => DCtr11("L", DVar())
      case _             => sys.error(s"getG01: $n, $pn")
    }
    override def getG11(n: String, pn: String): DExp2 =
      sys.error(s"getG11: $n, $pn")
    override def getG20(n: String, pn: String): DExp2 = (n, pn) match {
      case ("flip", "B") => DCtr211_2("B", DGCall1("flip"), DGCall1("flip"))
      case _             => sys.error(s"getG20: $n, $pn")
    }
  }
}

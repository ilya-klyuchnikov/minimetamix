package foetus

object aux {

  val debug = false

  def variant(x: String, ys: List[String]): String = {
    def var1(x: String, ys: List[String]): String = ys match {
      case Nil => x
      case y :: ys1 => if (x == y) var1(x + "'", ys) else var1(x, ys1)
    }
    val res = var1(x, ys)
    res
  }

  def flatMap0[A, B](result: List[A], f: B => List[A], list: List[B]): List[A] =
    list.foldLeft(result)((result1, el) => f(el) ++ result1)

  def flatMap[A, B](f: B => List[A], list: List[B]): List[A] =
    flatMap0(Nil, f, list)

  def union[A](set1: List[A], set2: List[A]): List[A] = set2 match {
    case Nil =>
      set1
    case el :: set2 =>
      if (set1.contains(el)) union(set1, set2) else union(el :: set1, set2)
  }

  def dropNth[A](i: Int, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case x :: xs => if (i == 0) xs else x :: dropNth(i - 1, xs)
  }

  def upto(l: Int, u: Int): List[Int] =
    if (l >= u) Nil else l :: upto(l + 1, u)

  def log(s: String) {
    if (debug) {
      println(s)
    }
  }
}

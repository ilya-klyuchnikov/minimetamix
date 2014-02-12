package foetus

object aux {
  
  def flatMap0[A, B](result: List[A], f: B => List[A], list: List[B]): List[A] =
    list.foldLeft(result)((result1, el) => f(el) ++ result1)
  
  // a bit different result, I am not sure - does it make sense or not?
  def flatMap[A, B](f: B => List[A], list: List[B]): List[A] =
    flatMap0(Nil, f, list)
}

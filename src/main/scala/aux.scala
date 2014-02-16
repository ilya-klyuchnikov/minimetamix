package foetus

object aux {

  val debug = false



  def log(s: String) {
    if (debug) {
      println(s)
    }
  }
}

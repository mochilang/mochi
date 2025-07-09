object sort_stable {
  val items = List(Map("n" -> (1), "v" -> ("a")), Map("n" -> (1), "v" -> ("b")), Map("n" -> (2), "v" -> ("c")))
  val result = (for { i <- items } yield i("v")).sortBy(i => i("n"))
  def main(args: Array[String]): Unit = {
    println((result))
  }
}

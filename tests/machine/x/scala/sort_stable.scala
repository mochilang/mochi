object sort_stable {
  val items = List[Map[String, Any]](Map[String, Any]("n" -> (1), "v" -> ("a")), Map[String, Any]("n" -> (1), "v" -> ("b")), Map[String, Any]("n" -> (2), "v" -> ("c")))
  val result = (for { i <- items } yield i("v")).sortBy(i => i("n"))
  def main(args: Array[String]): Unit = {
    println((result))
  }
}

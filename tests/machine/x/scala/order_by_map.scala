object order_by_map {
  def main(args: Array[String]): Unit = {
    val data = List(Map("a" -> (1), "b" -> (2)), Map("a" -> (1), "b" -> (1)), Map("a" -> (0), "b" -> (5)))
    val sorted = (for { x <- data } yield x).sortBy(x => Map("a" -> (x("a")), "b" -> (x("b"))))
    println((sorted))
  }
}

object order_by_map {
  val data = List(Map("a" -> (1), "b" -> (2)), Map("a" -> (1), "b" -> (1)), Map("a" -> (0), "b" -> (5)))
  val sorted = (for { x <- data } yield x).sortBy(x => (x("a"), x("b")))
  def main(args: Array[String]): Unit = {
    println((sorted))
  }
}

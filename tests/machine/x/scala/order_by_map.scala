object order_by_map {
  val data = List[Map[String, Int]](Map[String, Int]("a" -> (1), "b" -> (2)), Map[String, Int]("a" -> (1), "b" -> (1)), Map[String, Int]("a" -> (0), "b" -> (5)))
  val sorted = (for { x <- data } yield x).sortBy(x => (x("a"), x("b")))
  def main(args: Array[String]): Unit = {
    println((sorted))
  }
}

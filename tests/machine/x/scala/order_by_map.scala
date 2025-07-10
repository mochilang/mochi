object order_by_map {
  case class Auto1(a: Int, b: Int)

  val data = List[Auto1](Auto1(a = 1, b = 2), Auto1(a = 1, b = 1), Auto1(a = 0, b = 5))
  val sorted = (for { x <- data } yield x).sortBy(x => Auto1(a = x.a, b = x.b))
  def main(args: Array[String]): Unit = {
    println((sorted))
  }
}

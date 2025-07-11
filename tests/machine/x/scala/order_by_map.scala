object order_by_map {
  case class Data(a: Int, b: Int)

  val data = List[Data](Data(a = 1, b = 2), Data(a = 1, b = 1), Data(a = 0, b = 5))
  val sorted = (for { x <- data } yield x).sortBy(x => (x.a, x.b))
  def main(args: Array[String]): Unit = {
    println((sorted))
  }
}

object query_sum_select {
  val nums = List(1, 2, 3)
  val result = (for { n <- nums; if n > 1 } yield n).sum
  def main(args: Array[String]): Unit = {
    println((result))
  }
}

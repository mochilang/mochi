object map_int_key {
  val m = Map[Int, String](1 -> ("a"), 2 -> ("b"))
  def main(args: Array[String]): Unit = {
    println((m(1)))
  }
}

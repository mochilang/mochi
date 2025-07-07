object map_int_key {
  def main(args: Array[String]): Unit = {
    val m = Map(1 -> "a", 2 -> "b")
    println(m(1))
  }
}

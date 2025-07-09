object map_index {
  def main(args: Array[String]): Unit = {
    val m = Map("a" -> (1), "b" -> (2))
    println((m("b")))
  }
}

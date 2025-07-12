object map_index {
  val m = Map("a" -> (1), "b" -> (2))
  def main(args: Array[String]): Unit = {
    println(m("b"))
  }
}

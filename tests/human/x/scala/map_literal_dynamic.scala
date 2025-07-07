object map_literal_dynamic {
  def main(args: Array[String]): Unit = {
    val x = 3
    val y = 4
    val m = Map("a" -> x, "b" -> y)
    println(m("a"), m("b"))
  }
}

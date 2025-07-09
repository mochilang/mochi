object map_in_operator {
  def main(args: Array[String]): Unit = {
    val m = Map(1 -> ("a"), 2 -> ("b"))
    println((m.contains(1)))
    println((m.contains(3)))
  }
}

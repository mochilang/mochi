object map_in_operator {
  val m = Map(1 -> ("a"), 2 -> ("b"))
  def main(args: Array[String]): Unit = {
    println((m.contains(1)))
    println((m.contains(3)))
  }
}

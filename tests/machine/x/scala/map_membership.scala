object map_membership {
  def main(args: Array[String]): Unit = {
    val m = Map("a" -> (1), "b" -> (2))
    println((m.contains("a")))
    println((m.contains("c")))
  }
}

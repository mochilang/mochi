object map_membership {
  val m = Map[String, Int]("a" -> (1), "b" -> (2))
  def main(args: Array[String]): Unit = {
    println((m.contains("a")))
    println((m.contains("c")))
  }
}

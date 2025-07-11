object map_membership {
  case class M(a: Int, b: Int)

  val m = M(a = 1, b = 2)
  def main(args: Array[String]): Unit = {
    println((m.contains("a")))
    println((m.contains("c")))
  }
}

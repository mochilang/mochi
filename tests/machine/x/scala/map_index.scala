object map_index {
  case class Auto1(a: Int, b: Int)

  val m = Auto1(a = 1, b = 2)
  def main(args: Array[String]): Unit = {
    println((m("b")))
  }
}

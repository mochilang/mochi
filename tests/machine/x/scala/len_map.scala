object len_map {
  case class Auto1(a: Int, b: Int)

  def main(args: Array[String]): Unit = {
    println((Auto1(a = 1, b = 2).size))
  }
}

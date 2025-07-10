object values_builtin {
  case class Auto1(a: Int, b: Int, c: Int)

  val m = Auto1(a = 1, b = 2, c = 3)
  def main(args: Array[String]): Unit = {
    println((m.values.toList))
  }
}

object values_builtin {
  case class M(a: Int, b: Int, c: Int)

  val m = M(a = 1, b = 2, c = 3)
  def main(args: Array[String]): Unit = {
    println((m.values.toList))
  }
}

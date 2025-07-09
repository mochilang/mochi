object values_builtin {
  val m = Map("a" -> (1), "b" -> (2), "c" -> (3))
  def main(args: Array[String]): Unit = {
    println((m.values.toList))
  }
}

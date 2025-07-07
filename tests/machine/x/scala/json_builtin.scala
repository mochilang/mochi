object json_builtin {
  def main(args: Array[String]): Unit = {
    val m = Map(a -> 1, b -> 2)
    json(m)
  }
}

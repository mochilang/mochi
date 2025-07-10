object append_builtin {
  val a = List[Int](1, 2)
  def main(args: Array[String]): Unit = {
    println((a :+ 3))
  }
}

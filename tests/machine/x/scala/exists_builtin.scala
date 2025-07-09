object exists_builtin {
  def main(args: Array[String]): Unit = {
    val data = List(1, 2)
    val flag = (for { x <- data; if x == 1 } yield x).nonEmpty
    println(flag)
  }
}

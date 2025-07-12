object exists_builtin {
  val data = List(1, 2)
  val flag = (for { x <- data; if x == 1 } yield x).nonEmpty
  def main(args: Array[String]): Unit = {
    println(flag)
  }
}

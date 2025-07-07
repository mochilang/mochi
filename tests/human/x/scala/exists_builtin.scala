object exists_builtin {
  def main(args: Array[String]): Unit = {
    val data = List(1,2)
    val flag = data.exists(_ == 1)
    println(flag)
  }
}

object in_operator {
  def main(args: Array[String]): Unit = {
    val xs = List(1, 2, 3)
    println(xs.contains(2))
    println(!(xs.contains(5)))
  }
}

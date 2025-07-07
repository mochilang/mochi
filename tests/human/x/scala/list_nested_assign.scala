object list_nested_assign {
  def main(args: Array[String]): Unit = {
    val matrix = Array(Array(1,2), Array(3,4))
    matrix(1)(0) = 5
    println(matrix(1)(0))
  }
}

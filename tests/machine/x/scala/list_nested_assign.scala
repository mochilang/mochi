object list_nested_assign {
  def main(args: Array[String]): Unit = {
    var matrix = scala.collection.mutable.ArrayBuffer[List[Int]](List[Int](1, 2), List[Int](3, 4))
    val _tmp0 = matrix(1).updated(0, 5)
    matrix = matrix.updated(1, _tmp0)
    println((matrix(1)(0)))
  }
}

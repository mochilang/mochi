object list_nested_assign {
  def main(args: Array[String]): Unit = {
    var matrix = scala.collection.mutable.ArrayBuffer(List(1, 2), List(3, 4))
    matrix(1).update(0, 5)
    println((matrix(1)(0)))
  }
}

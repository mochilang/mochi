object list_assign {
  def main(args: Array[String]): Unit = {
    var nums = scala.collection.mutable.ArrayBuffer[Int](1, 2)
    nums(1) = 3
    println((nums(1)))
  }
}

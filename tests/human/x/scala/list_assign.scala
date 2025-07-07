object list_assign {
  def main(args: Array[String]): Unit = {
    val nums = scala.collection.mutable.ArrayBuffer(1,2)
    nums(1) = 3
    println(nums(1))
  }
}

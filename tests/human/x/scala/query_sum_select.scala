object query_sum_select {
  def main(args: Array[String]): Unit = {
    val nums = List(1,2,3)
    val result = nums.filter(_ > 1).map(identity).sum
    println(result)
  }
}

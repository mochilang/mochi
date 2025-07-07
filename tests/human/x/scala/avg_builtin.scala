object avg_builtin {
  def main(args: Array[String]): Unit = {
    val nums = List(1, 2, 3)
    val avg = nums.sum.toDouble / nums.size
    println(avg)
  }
}

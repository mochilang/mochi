object pure_global_fold {
  def inc(x: Int): Int = {
    return x + k
  }
  
  def main(args: Array[String]): Unit = {
    val k = 2
    println(inc(3))
  }
}

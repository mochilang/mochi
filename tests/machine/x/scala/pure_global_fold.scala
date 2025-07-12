object pure_global_fold {
  val k = 2
  def inc(x: Int): Int = x + k
  
  def main(args: Array[String]): Unit = {
    println(inc(3))
  }
}

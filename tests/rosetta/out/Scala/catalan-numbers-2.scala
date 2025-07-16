object catalan_numbers_2 {
  def catalanRec(n: Int): Int = {
    if (n == 0) {
      return 1
    }
    var t1 = 2 * n
    var t2 = t1 - 1
    var t3 = 2 * t2
    var t5 = t3 * catalanRec(n - 1)
    return (t5 / (n + 1)).toInt
  }
  
  def main() = {
    for(i <- 1 until 16) {
      println(catalanRec(i).toString)
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}

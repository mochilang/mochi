object bernoulli_numbers {
  def bernoulli(n: Int): bigrat = {
    var a: List[bigrat] = scala.collection.mutable.ArrayBuffer[Any]()
    var m = 0
    while (m <= n) {
      a = a :+ 1.asInstanceOf[bigrat] / ((m + 1).asInstanceOf[bigrat])
      var j = m
      while (j >= 1) {
        a(j - 1) = (j.asInstanceOf[bigrat]) * ((a).apply(j - 1) - (a).apply(j))
        j -= 1
      }
      m += 1
    }
    return (a).apply(0)
  }
  
  def main(args: Array[String]): Unit = {
    for(i <- 0 until 61) {
      val b = bernoulli(i)
      if (num(b) != 0) {
        val numStr = num(b).toString
        val denStr = denom(b).toString
        println("B(" + i.toString.reverse.padTo(2, ' ').reverse + ") =" + numStr.padStart(45, " ") + "/" + denStr)
      }
    }
  }
}

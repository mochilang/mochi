object averages_median_1 {
  def sortFloat(xs: List[Double]): List[Double] = {
    var arr = xs
    var n = arr.length
    var i = 0
    while (i < n) {
      var j = 0
      while (j < n - 1) {
        if ((arr).apply(j) > (arr).apply(j + 1)) {
          val tmp = (arr).apply(j)
          arr(j) = (arr).apply(j + 1)
          arr(j + 1) = tmp
        }
        j += 1
      }
      i += 1
    }
    return arr
  }
  
  def median(a: List[Double]): Double = {
    var arr = sortFloat(a)
    val half = (arr.length / 2).toInt
    var m = (arr).apply(half)
    if (arr.length % 2 == 0) {
      m = (m + (arr).apply(half - 1)) / 2
    }
    return m
  }
  
  def main(args: Array[String]): Unit = {
    println(median(List(3, 1, 4, 1)).toString)
    println(median(List(3, 1, 4, 1, 5)).toString)
  }
}

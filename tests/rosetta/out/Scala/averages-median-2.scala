object averages_median_2 {
  def sel(list: List[Double], k: Int): Double = {
    var i = 0
    while (i <= k) {
      var minIndex = i
      var j = i + 1
      while (j < list.length) {
        if ((list).apply(j) < (list).apply(minIndex)) {
          minIndex = j
        }
        j += 1
      }
      val tmp = (list).apply(i)
      list(i) = (list).apply(minIndex)
      list(minIndex) = tmp
      i += 1
    }
    return (list).apply(k)
  }
  
  def median(a: List[Double]): Double = {
    var arr = a
    val half = (arr.length / 2).toInt
    val med = sel(arr, half)
    if (arr.length % 2 == 0) {
      return (med + (arr).apply(half - 1)) / 2
    }
    return med
  }
  
  def main(args: Array[String]): Unit = {
    println(median(List(3, 1, 4, 1)).toString)
    println(median(List(3, 1, 4, 1, 5)).toString)
  }
}

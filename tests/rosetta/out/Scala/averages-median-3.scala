object averages_median_3 {
  def qsel(a: List[Double], k: Int): Double = {
    var arr = a
    while (arr.length > 1) {
      var px = now() % arr.length
      var pv = (arr).apply(px)
      val last = arr.length - 1
      val tmp = (arr).apply(px)
      arr(px) = (arr).apply(last)
      arr(last) = tmp
      px = 0
      var i = 0
      while (i < last) {
        val v = (arr).apply(i)
        if (v < pv) {
          val tmp2 = (arr).apply(px)
          arr(px) = (arr).apply(i)
          arr(i) = tmp2
          px += 1
        }
        i += 1
      }
      if (px == k) {
        return pv
      }
      if (k < px) {
        arr = arr.slice(0, px)
      } else {
        val tmp2 = (arr).apply(px)
        arr(px) = pv
        arr(last) = tmp2
        arr = arr.slice((px + 1), arr.length)
        k -= (px + 1)
      }
    }
    return (arr).apply(0)
  }
  
  def median(list: List[Double]): Double = {
    var arr = list
    val half = (arr.length / 2).toInt
    val med = qsel(arr, half)
    if (arr.length % 2 == 0) {
      return (med + qsel(arr, half - 1)) / 2
    }
    return med
  }
  
  def main(args: Array[String]): Unit = {
    println(median(List(3, 1, 4, 1)).toString)
    println(median(List(3, 1, 4, 1, 5)).toString)
  }
}

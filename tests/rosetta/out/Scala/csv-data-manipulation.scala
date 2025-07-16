object csv_data_manipulation {
  def join(xs: List[String], sep: String): String = {
    var res = ""
    var i = 0
    while (i < xs.length) {
      if (i > 0) {
        res += sep
      }
      res += (xs).apply(i)
      i += 1
    }
    return res
  }
  
  def parseIntStr(str: String): Int = {
    var i = 0
    var neg = false
    if (str.length > 0 && str.substring(0, 1) == "-") {
      neg = true
      i = 1
    }
    var n = 0
    val digits = Map("0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9)
    while (i < str.length) {
      n = n * 10 + (digits).apply(str.substring(i, i + 1))
      i += 1
    }
    if (neg) {
      n = -n
    }
    return n
  }
  
  def main(args: Array[String]): Unit = {
    var rows: List[List[String]] = scala.collection.mutable.ArrayBuffer(List("A", "B", "C"), List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"))
    rows(0) = (rows).apply(0) :+ "SUM"
    var i = 1
    while (i < rows.length) {
      var sum = 0
      for(s <- (rows).apply(i)) {
        sum += parseIntStr(s)
      }
      rows(i) = (rows).apply(i) :+ sum.toString
      i += 1
    }
    for(r <- rows) {
      println(join(r, ","))
    }
  }
}

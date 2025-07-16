object _100_doors_3 {
  def main(args: Array[String]): Unit = {
    var result = ""
    for(i <- 1 until 101) {
      var j = 1
      while (j * j < i) {
        j += 1
      }
      if (j * j == i) {
        result += "O"
      } else {
        result += "-"
      }
    }
    println(result)
  }
}

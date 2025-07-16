object _100_doors {
  def main(args: Array[String]): Unit = {
    var doors = scala.collection.mutable.ArrayBuffer[Any]()
    for(i <- 0 until 100) {
      doors = doors :+ false
    }
    for(pass <- 1 until 101) {
      var idx = pass - 1
      while (idx < 100) {
        doors(idx) = !((doors).apply(idx) != null)
        idx += pass
      }
    }
    for(row <- 0 until 10) {
      var line = ""
      for(col <- 0 until 10) {
        val idx = row * 10 + col
        if ((doors).apply(idx) != null) {
          line += "1"
        } else {
          line += "0"
        }
        if (col < 9) {
          line += " "
        }
      }
      println(line)
    }
  }
}

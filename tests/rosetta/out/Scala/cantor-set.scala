object cantor_set {
  val width = 81
  val height = 5
  def setChar(s: String, idx: Int, ch: String): String = s.substring(0, idx) + ch + s.substring(idx + 1, s.length)
  
  def main(args: Array[String]): Unit = {
    var lines: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    for(i <- 0 until height) {
      var row = ""
      var j = 0
      while (j < width) {
        row += "*"
        j += 1
      }
      lines = lines :+ row
    }
    var stack: List[Map[String, Int]] = scala.collection.mutable.ArrayBuffer(Map("start" -> 0, "len" -> width, "index" -> 1))
    while (stack.length > 0) {
      var frame = (stack).apply(stack.length - 1)
      stack = stack.slice(0, stack.length - 1)
      val start = (frame).apply("start")
      val lenSeg = (frame).apply("len")
      val index = (frame).apply("index")
      val seg = (lenSeg / 3).toInt
      if (seg == 0) {
        // continue
      }
      var i = index
      while (i < height) {
        var j = start + seg
        while ((j < start + 2).asInstanceOf[Int] * seg) {
          lines(i) = setChar((lines).apply(i), j, " ")
          j += 1
        }
        i += 1
      }
      stack = stack :+ Map("start" -> start, "len" -> seg, "index" -> index + 1)
      stack = stack :+ Map("start" -> start + seg * 2, "len" -> seg, "index" -> index + 1)
    }
    for(line <- lines) {
      println(line)
    }
  }
}

object bitmap_read_an_image_through_a_pipe {
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
  
  def splitWs(s: String): List[String] = {
    var parts: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var cur = ""
    var i = 0
    while (i < s.length) {
      val ch = s.substring(i, i + 1)
      if (ch == " " || ch == "\n" || ch == "\t" || ch == "\r") {
        if (cur.length > 0) {
          parts = parts :+ cur
          cur = ""
        }
      } else {
        cur += ch
      }
      i += 1
    }
    if (cur.length > 0) {
      parts = parts :+ cur
    }
    return parts
  }
  
  def parsePpm(data: String): Map[String, any] = {
    val toks = splitWs(data)
    if (toks.length < 4) {
      return Map("err" -> true)
    }
    val magic = (toks).apply(0)
    val w = parseIntStr((toks).apply(1))
    val h = parseIntStr((toks).apply(2))
    val maxv = parseIntStr((toks).apply(3))
    var px: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 4
    while (i < toks.length) {
      px = px :+ parseIntStr((toks).apply(i))
      i += 1
    }
    return Map("magic" -> magic, "w" -> w, "h" -> h, "max" -> maxv, "px" -> px)
  }
  
  def main(args: Array[String]): Unit = {
    val ppmData = "P3\n2 2\n1\n0 1 1 0 1 0 0 1 1 1 0 0\n"
    val img = parsePpm(ppmData)
    println((("width=" + img("w").toString).asInstanceOf[Int] + " height=").asInstanceOf[Int] + img("h").toString)
  }
}

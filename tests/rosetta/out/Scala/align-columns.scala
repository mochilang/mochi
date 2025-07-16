object align_columns {
  def split(s: String, sep: String): List[String] = {
    var parts: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var cur = ""
    var i = 0
    while (i < s.length) {
      if ((sep.length > 0 && i + sep.length).asInstanceOf[Int] <= s.length && s.substring(i, i + sep.length) == sep) {
        parts = parts :+ cur
        cur = ""
        i += sep.length
      } else {
        cur += s.substring(i, i + 1)
        i += 1
      }
    }
    parts = parts :+ cur
    return parts
  }
  
  def rstripEmpty(words: List[String]): List[String] = {
    var n = words.length
    while (n > 0 && (words).apply(n - 1) == "") {
      n -= 1
    }
    return words.slice(0, n)
  }
  
  def spaces(n: Int): String = {
    var out = ""
    var i = 0
    while (i < n) {
      out += " "
      i += 1
    }
    return out
  }
  
  def pad(word: String, width: Int, align: Int): String = {
    val diff = width - word.length
    if (align == 0) {
      return word + spaces(diff)
    }
    if (align == 2) {
      return spaces(diff) + word
    }
    var left = (diff / 2).toInt
    var right = diff - left
    return spaces(left) + word + spaces(right)
  }
  
  def newFormatter(text: String): Map[String, any] = {
    var lines = split(text, "\n")
    var fmtLines: List[List[String]] = scala.collection.mutable.ArrayBuffer[Any]()
    var width: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < lines.length) {
      if ((lines).apply(i).length == 0) {
        i += 1
        // continue
      }
      var words = rstripEmpty(split((lines).apply(i), "$"))
      fmtLines = fmtLines :+ words
      var j = 0
      while (j < words.length) {
        val wlen = (words).apply(j).length
        if (j == width.length) {
          width = width :+ wlen
        } else {
          if (wlen > (width).apply(j)) {
            width(j) = wlen
          }
        }
        j += 1
      }
      i += 1
    }
    return Map("text" -> fmtLines, "width" -> width)
  }
  
  def printFmt(f: Map[String, any], align: Int) = {
    val lines = (f).apply("text").asInstanceOf[List[List[String]]]
    val width = (f).apply("width").asInstanceOf[List[Int]]
    var i = 0
    while (i < lines.length) {
      val words = (lines).apply(i)
      var line = ""
      var j = 0
      while (j < words.length) {
        line = line + pad((words).apply(j), (width).apply(j), align) + " "
        j += 1
      }
      println(line)
      i += 1
    }
    println("")
  }
  
  def main(args: Array[String]): Unit = {
    val text = "Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" + "are$delineated$by$a$single$'dollar'$character,$write$a$program\n" + "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n" + "column$are$separated$by$at$least$one$space.\n" + "Further,$allow$for$each$word$in$a$column$to$be$either$left\n" + "justified,$right$justified,$or$center$justified$within$its$column."
    val f = newFormatter(text)
    printFmt(f, 0)
    printFmt(f, 1)
    printFmt(f, 2)
  }
}

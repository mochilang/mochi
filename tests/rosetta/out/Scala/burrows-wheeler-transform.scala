object burrows_wheeler_transform {
  val stx = "\x02"
  val etx = "\x03"
  def contains(s: String, ch: String): Boolean = {
    var i = 0
    while (i < s.length) {
      if (s.substring(i, i + 1) == ch) {
        return true
      }
      i += 1
    }
    return false
  }
  
  def sortStrings(xs: List[String]): List[String] = {
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
  
  def bwt(s: String): Map[String, any] = {
    if (contains(s, stx) || contains(s, etx)) {
      return Map("err" -> true, "res" -> "")
    }
    s = stx + s + etx
    val le = s.length
    var table: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < le) {
      val rot = s.substring(i, le) + s.substring(0, i)
      table = table :+ rot
      i += 1
    }
    table = sortStrings(table)
    var last = ""
    i = 0
    while (i < le) {
      last += (table).apply(i).substring(le - 1, le)
      i += 1
    }
    return Map("err" -> false, "res" -> last)
  }
  
  def ibwt(r: String): String = {
    val le = r.length
    var table: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < le) {
      table = table :+ ""
      i += 1
    }
    var n = 0
    while (n < le) {
      i = 0
      while (i < le) {
        table(i) = r.substring(i, i + 1) + (table).apply(i)
        i += 1
      }
      table = sortStrings(table)
      n += 1
    }
    i = 0
    while (i < le) {
      if ((table).apply(i).substring(le - 1, le) == etx) {
        return (table).apply(i).substring(1, le - 1)
      }
      i += 1
    }
    return ""
  }
  
  def makePrintable(s: String): String = {
    var out = ""
    var i = 0
    while (i < s.length) {
      val ch = s.substring(i, i + 1)
      if (ch == stx) {
        out += "^"
      } else {
        if (ch == etx) {
          out += "|"
        } else {
          out += ch
        }
      }
      i += 1
    }
    return out
  }
  
  def main() = {
    val examples = List("banana", "appellee", "dogwood", "TO BE OR NOT TO BE OR WANT TO BE OR NOT?", "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES", "\x02ABC\x03")
    for(t <- examples) {
      println(makePrintable(t))
      val res = bwt(t)
      if ((res).apply("err") != null) {
        println(" --> ERROR: String can't contain STX or ETX")
        println(" -->")
      } else {
        val enc = (res).apply("res").toString
        println(" --> " + makePrintable(enc))
        val r = ibwt(enc)
        println(" --> " + r)
      }
      println("")
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}

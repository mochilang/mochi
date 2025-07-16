object abc_problem {
  def fields(s: String): List[String] = {
    var res: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var cur = ""
    var i = 0
    while (i < s.length) {
      val c = s.substring(i, i + 1)
      if (c == " ") {
        if (cur.length > 0) {
          res = res :+ cur
          cur = ""
        }
      } else {
        cur += c
      }
      i += 1
    }
    if (cur.length > 0) {
      res = res :+ cur
    }
    return res
  }
  
  def canSpell(word: String, blks: List[String]): Boolean = {
    if (word.length == 0) {
      return true
    }
    val c = lower(word.substring(0, 1))
    var i = 0
    while (i < blks.length) {
      val b = (blks).apply(i)
      if (c == lower(b.substring(0, 1)) || c == lower(b.substring(1, 2))) {
        var rest: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
        var j = 0
        while (j < blks.length) {
          if (j != i) {
            rest = rest :+ (blks).apply(j)
          }
          j += 1
        }
        if (canSpell(word.substring(1, word.length), rest)) {
          return true
        }
      }
      i += 1
    }
    return false
  }
  
  def newSpeller(blocks: String): (String) => Boolean = {
    val bl = fields(blocks)
    return (w: String) => canSpell(w, bl)
  }
  
  def main() = {
    val sp = newSpeller("BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM")
    for(word <- List("A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE")) {
      println(word + " " + sp(word).toString)
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}

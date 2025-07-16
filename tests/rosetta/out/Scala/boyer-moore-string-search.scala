object boyer_moore_string_search {
  def indexOfStr(h: String, n: String): Int = {
    val hlen = h.length
    val nlen = n.length
    if (nlen == 0) {
      return 0
    }
    var i = 0
    while (i <= hlen - nlen) {
      if (h.substring(i, i + nlen) == n) {
        return i
      }
      i += 1
    }
    return -1
  }
  
  def stringSearchSingle(h: String, n: String): Int = indexOfStr(h, n)
  
  def stringSearch(h: String, n: String): List[Int] = {
    var result: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var start = 0
    val hlen = h.length
    val nlen = n.length
    while (start < hlen) {
      val idx = indexOfStr(h.substring(start, hlen), n)
      if (idx >= 0) {
        result = result :+ start + idx
        start = start + idx + nlen
      } else {
        return
      }
    }
    return result
  }
  
  def display(nums: List[Int]): String = {
    var s = "["
    var i = 0
    while (i < nums.length) {
      if (i > 0) {
        s += ", "
      }
      s += (nums).apply(i).toString
      i += 1
    }
    s += "]"
    return s
  }
  
  def main() = {
    val texts = List("GCTAGCTCTACGAGTCTA", "GGCTATAATGCGTA", "there would have been a time for such a word", "needle need noodle needle", "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages", "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk.")
    val patterns = List("TCTA", "TAATAAA", "word", "needle", "and", "alfalfa")
    var i = 0
    while (i < texts.length) {
      println((("text" + i + 1.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + (texts).apply(i))
      i += 1
    }
    println("")
    var j = 0
    while (j < texts.length) {
      val idxs = stringSearch((texts).apply(j), (patterns).apply(j))
      println((((("Found \"" + (patterns).apply(j)).asInstanceOf[Int] + "\" in 'text").asInstanceOf[Int] + j + 1.toString).asInstanceOf[Int] + "' at indexes ").asInstanceOf[Int] + display(idxs))
      j += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}

object anagrams {
  def sortRunes(s: String): String = {
    var arr: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < s.length) {
      arr = arr :+ s.substring(i, i + 1)
      i += 1
    }
    var n = arr.length
    var m = 0
    while (m < n) {
      var j = 0
      while (j < n - 1) {
        if ((arr).apply(j) > (arr).apply(j + 1)) {
          val tmp = (arr).apply(j)
          arr(j) = (arr).apply(j + 1)
          arr(j + 1) = tmp
        }
        j += 1
      }
      m += 1
    }
    var out = ""
    i = 0
    while (i < n) {
      out += (arr).apply(i)
      i += 1
    }
    return out
  }
  
  def sortStrings(xs: List[String]): List[String] = {
    var res: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var tmp = xs
    while (tmp.length > 0) {
      var min = (tmp).apply(0)
      var idx = 0
      var i = 1
      while (i < tmp.length) {
        if ((tmp).apply(i) < min) {
          min = (tmp).apply(i)
          idx = i
        }
        i += 1
      }
      res = res :+ min
      var out: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
      var j = 0
      while (j < tmp.length) {
        if (j != idx) {
          out = out :+ (tmp).apply(j)
        }
        j += 1
      }
      tmp = out
    }
    return res
  }
  
  def main() = {
    val words = List("abel", "able", "bale", "bela", "elba", "alger", "glare", "lager", "large", "regal", "angel", "angle", "galen", "glean", "lange", "caret", "carte", "cater", "crate", "trace", "elan", "lane", "lean", "lena", "neal", "evil", "levi", "live", "veil", "vile")
    var groups: Map[String, List[String]] = scala.collection.mutable.Map()
    var maxLen = 0
    for(w <- words) {
      val k = sortRunes(w)
      if (!(groups.contains(k))) {
        groups(k) = List(w)
      } else {
        groups(k) = (groups).apply(k) :+ w
      }
      if ((groups).apply(k).length > maxLen) {
        maxLen = (groups).apply(k).length
      }
    }
    var printed: Map[String, Boolean] = scala.collection.mutable.Map()
    for(w <- words) {
      val k = sortRunes(w)
      if ((groups).apply(k).length == maxLen) {
        if (!(printed.contains(k))) {
          var g = sortStrings((groups).apply(k))
          var line = "[" + (g).apply(0)
          var i = 1
          while (i < g.length) {
            line = (line + " ").asInstanceOf[Int] + (g).apply(i)
            i += 1
          }
          line += "]"
          println(line)
          printed(k) = true
        }
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}

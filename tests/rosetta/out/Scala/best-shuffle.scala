object best_shuffle {
  def nextRand(seed: Int): Int = (seed * 1664525 + 1013904223) % 2147483647
  
  def shuffleChars(s: String, seed: Int): List[any] = {
    var chars: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < s.length) {
      chars = chars :+ s.substring(i, i + 1)
      i += 1
    }
    var sd = seed
    var idx = chars.length - 1
    while (idx > 0) {
      sd = nextRand(sd)
      var j = sd % (idx + 1)
      val tmp = (chars).apply(idx)
      chars(idx) = (chars).apply(j)
      chars(j) = tmp
      idx -= 1
    }
    var res = ""
    i = 0
    while (i < chars.length) {
      res += (chars).apply(i)
      i += 1
    }
    return List(res, sd)
  }
  
  def bestShuffle(s: String, seed: Int): List[any] = {
    val r = shuffleChars(s, seed)
    var t = (r).apply(0)
    var sd = (r).apply(1)
    var arr: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < t.length) {
      arr = arr :+ t.substring(i, i + 1)
      i += 1
    }
    i = 0
    while (i < arr.length) {
      var j = 0
      while (j < arr.length) {
        if (i != j && (arr).apply(i) != s.substring(j, j + 1) && (arr).apply(j) != s.substring(i, i + 1)) {
          val tmp = (arr).apply(i)
          arr(i) = (arr).apply(j)
          arr(j) = tmp
          return
        }
        j += 1
      }
      i += 1
    }
    var count = 0
    i = 0
    while (i < arr.length) {
      if ((arr).apply(i) == s.substring(i, i + 1)) {
        count += 1
      }
      i += 1
    }
    var out = ""
    i = 0
    while (i < arr.length) {
      out += (arr).apply(i)
      i += 1
    }
    return List(out, sd, count)
  }
  
  def main() = {
    val ts = List("abracadabra", "seesaw", "elk", "grrrrrr", "up", "a")
    var seed = 1
    var i = 0
    while (i < ts.length) {
      val r = bestShuffle((ts).apply(i), seed)
      val shuf = (r).apply(0)
      seed = (r).apply(1)
      val cnt = (r).apply(2)
      println((ts).apply(i) + " -> " + shuf + " (" + cnt.toString + ")")
      i += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}

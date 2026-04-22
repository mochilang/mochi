object Main {
  def ladders(begin: String, end: String, words: Vector[String]): Vector[Vector[String]] = {
    val wordSet = words.toSet
    if (!wordSet.contains(end)) return Vector()
    var parents = Map.empty[String, Vector[String]]
    var level = Set(begin)
    var visited = Set(begin)
    var found = false
    while (level.nonEmpty && !found) {
      var next = Set.empty[String]
      for (word <- level.toVector.sorted) {
        val arr = word.toCharArray
        for (i <- arr.indices) {
          val orig = arr(i)
          for (c <- 'a' to 'z') {
            if (c != orig) {
              arr(i) = c
              val nw = new String(arr)
              if (wordSet.contains(nw) && !visited.contains(nw)) {
                next += nw
                parents = parents.updated(nw, parents.getOrElse(nw, Vector()) :+ word)
                if (nw == end) found = true
              }
            }
          }
          arr(i) = orig
        }
      }
      visited ++= next
      level = next
    }
    if (!found) return Vector()
    def backtrack(word: String): Vector[Vector[String]] = {
      if (word == begin) Vector(Vector(begin))
      else parents.getOrElse(word, Vector()).sorted.flatMap(p => backtrack(p).map(_ :+ word))
    }
    backtrack(end).sortBy(_.mkString("->"))
  }
  def fmt(paths: Vector[Vector[String]]): String = (Vector(paths.length.toString) ++ paths.map(_.mkString("->"))).mkString("\n")
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toVector
    if (lines.nonEmpty) {
      var idx = 1
      val tc = lines(0).trim.toInt
      val out = (0 until tc).map { _ =>
        val begin = lines(idx); idx += 1
        val end = lines(idx); idx += 1
        val n = lines(idx).trim.toInt; idx += 1
        val words = lines.slice(idx, idx + n); idx += n
        fmt(ladders(begin, end, words))
      }
      print(out.mkString("\n\n"))
    }
  }
}

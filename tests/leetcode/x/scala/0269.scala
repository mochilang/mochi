import scala.collection.mutable

object Main {
  def solve(words: Array[String]): String = {
    val chars = words.mkString("").toSet.toSeq.sorted
    val adj = mutable.Map[Char, mutable.TreeSet[Char]]()
    val indeg = mutable.Map[Char, Int]()
    for (c <- chars) {
      adj(c) = mutable.TreeSet[Char]()
      indeg(c) = 0
    }
    for (i <- 0 until words.length - 1) {
      val a = words(i)
      val b = words(i + 1)
      val m = math.min(a.length, b.length)
      if (a.take(m) == b.take(m) && a.length > b.length) return ""
      var done = false
      var j = 0
      while (j < m && !done) {
        if (a(j) != b(j)) {
          if (!adj(a(j)).contains(b(j))) {
            adj(a(j)) += b(j)
            indeg(b(j)) = indeg(b(j)) + 1
          }
          done = true
        }
        j += 1
      }
    }
    val pq = mutable.PriorityQueue.empty[Char](using Ordering.Char.reverse)
    for (c <- chars if indeg(c) == 0) pq.enqueue(c)
    val out = new StringBuilder
    while (pq.nonEmpty) {
      val c = pq.dequeue()
      out += c
      for (nei <- adj(c)) {
        indeg(nei) = indeg(nei) - 1
        if (indeg(nei) == 0) pq.enqueue(nei)
      }
    }
    if (out.length == chars.length) out.toString else ""
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.isEmpty) return
    val t = lines(0).trim.toInt
    val out = new mutable.ArrayBuffer[String]()
    var idx = 1
    for (_ <- 0 until t) {
      val n = lines(idx).trim.toInt
      idx += 1
      out += solve(lines.slice(idx, idx + n).map(_.trim))
      idx += n
    }
    print(out.mkString("\n"))
  }
}

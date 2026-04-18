import scala.io.Source

object Main extends App {
  def longest(s: String): Int = {
    val last = scala.collection.mutable.Map.empty[Char, Int]
    var left = 0
    var best = 0
    for ((ch, right) <- s.zipWithIndex) {
      last.get(ch).foreach(pos => if (pos >= left) left = pos + 1)
      last(ch) = right
      best = math.max(best, right - left + 1)
    }
    best
  }
  val lines = Source.stdin.mkString.split("\n", -1).map(_.stripSuffix("\r"))
  if (lines.nonEmpty && lines(0).trim.nonEmpty) {
    val t = lines(0).trim.toInt
    print(lines.slice(1, 1 + t).map(longest).mkString("\n"))
  }
}

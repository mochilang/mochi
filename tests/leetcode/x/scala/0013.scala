import scala.io.Source

object Main extends App {
  val tokens = Source.stdin.mkString.split("\\s+").filter(_.nonEmpty)
  if (tokens.nonEmpty) {
    val values = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)
    def romanToInt(s: String): Int = {
      var total = 0
      for (i <- s.indices) {
        val cur = values(s(i))
        val next = if (i + 1 < s.length) values(s(i + 1)) else 0
        total += (if (cur < next) -cur else cur)
      }
      total
    }
    val t = tokens(0).toInt
    print(tokens.slice(1, 1 + t).map(romanToInt).mkString("\n"))
  }
}

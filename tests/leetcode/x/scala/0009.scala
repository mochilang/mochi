import scala.io.Source

object Main extends App {
  val tokens = Source.stdin.mkString.split("\\s+").filter(_.nonEmpty)
  if (tokens.nonEmpty) {
    val t = tokens(0).toInt
    def isPalindrome(x: Int): Boolean = {
      if (x < 0) false
      else {
        val original = x
        var n = x
        var rev = 0L
        while (n > 0) {
          rev = rev * 10 + (n % 10)
          n /= 10
        }
        rev == original
      }
    }
    print(tokens.slice(1, 1 + t).map(x => if (isPalindrome(x.toInt)) "true" else "false").mkString("\n"))
  }
}

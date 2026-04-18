import scala.io.Source

object Main extends App {
  val tokens = Source.stdin.mkString.split("\\s+").filter(_.nonEmpty)
  if (tokens.nonEmpty) {
    def isValid(s: String): Boolean = {
      val stack = scala.collection.mutable.ArrayBuffer.empty[Char]
      for (ch <- s) {
        if (ch == '(' || ch == '[' || ch == '{') {
          stack += ch
        } else {
          if (stack.isEmpty) return false
          val open = stack.remove(stack.length - 1)
          if ((ch == ')' && open != '(') ||
              (ch == ']' && open != '[') ||
              (ch == '}' && open != '{')) return false
        }
      }
      stack.isEmpty
    }
    val t = tokens(0).toInt
    print(tokens.slice(1, 1 + t).map(s => if (isValid(s)) "true" else "false").mkString("\n"))
  }
}

object Main {
  def calculate(expr: String): Int = {
    var result = 0
    var number = 0
    var sign = 1
    val stack = scala.collection.mutable.ArrayBuffer[Int]()
    for (ch <- expr) {
      if (ch >= '0' && ch <= '9') {
        number = number * 10 + (ch - '0')
      } else if (ch == '+' || ch == '-') {
        result += sign * number
        number = 0
        sign = if (ch == '+') 1 else -1
      } else if (ch == '(') {
        stack += result
        stack += sign
        result = 0
        number = 0
        sign = 1
      } else if (ch == ')') {
        result += sign * number
        number = 0
        val prevSign = stack.remove(stack.length - 1)
        val prevResult = stack.remove(stack.length - 1)
        result = prevResult + prevSign * result
      }
    }
    result + sign * number
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.isEmpty) return
    val t = lines(0).trim.toInt
    val out = (0 until t).map(i => calculate(lines(i + 1)).toString)
    print(out.mkString("\n"))
  }
}

object Main {
  val less20 = Array("", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
    "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen")
  val tens = Array("", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety")
  val thousands = Array("", "Thousand", "Million", "Billion")

  def helper(n: Int): String =
    if (n == 0) ""
    else if (n < 20) less20(n)
    else if (n < 100) tens(n / 10) + (if (n % 10 == 0) "" else " " + helper(n % 10))
    else less20(n / 100) + " Hundred" + (if (n % 100 == 0) "" else " " + helper(n % 100))

  def solve(num0: Int): String = {
    if (num0 == 0) return "Zero"
    var num = num0
    val parts = new scala.collection.mutable.ArrayBuffer[String]()
    var idx = 0
    while (num > 0) {
      val chunk = num % 1000
      if (chunk != 0) {
        var words = helper(chunk)
        if (thousands(idx) != "") words += " " + thousands(idx)
        parts.prepend(words)
      }
      num /= 1000
      idx += 1
    }
    parts.mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    if (lines.isEmpty) return
    val t = lines(0).trim.toInt
    val out = (0 until t).map(i => solve(lines(i + 1).trim.toInt))
    print(out.mkString("\n"))
  }
}

// https://www.spoj.com/problems/TEST
object Main {
  def main(args: Array[String]): Unit = {
    for (line <- scala.io.Source.stdin.getLines()) {
      val n = line.trim.toInt
      if (n == 42) return
      println(n)
    }
  }
}

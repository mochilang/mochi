import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Iterator.continually(StdIn.readLine()).takeWhile(_ != null).map(_.trim).filter(_.nonEmpty).toArray
    if (lines.isEmpty) return
    val t = lines(0).toInt
    print((0 until t).map(i => lines(i + 1)).mkString("\n\n"))
  }
}

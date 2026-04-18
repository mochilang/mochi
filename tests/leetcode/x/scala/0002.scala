import scala.io.Source

object Main extends App {
  val tokens = Source.stdin.mkString.split("\\s+").filter(_.nonEmpty)
  def addLists(a: Vector[Int], b: Vector[Int]): Vector[Int] = {
    var i = 0
    var j = 0
    var carry = 0
    val out = scala.collection.mutable.ArrayBuffer.empty[Int]
    while (i < a.length || j < b.length || carry > 0) {
      var sum = carry
      if (i < a.length) { sum += a(i); i += 1 }
      if (j < b.length) { sum += b(j); j += 1 }
      out += (sum % 10)
      carry = sum / 10
    }
    out.toVector
  }
  def fmt(a: Vector[Int]) = "[" + a.mkString(",") + "]"
  if (tokens.nonEmpty) {
    var idx = 1
    val t = tokens(0).toInt
    val lines = scala.collection.mutable.ArrayBuffer.empty[String]
    for (_ <- 0 until t) {
      val n = tokens(idx).toInt; idx += 1
      val a = tokens.slice(idx, idx + n).map(_.toInt).toVector; idx += n
      val m = tokens(idx).toInt; idx += 1
      val b = tokens.slice(idx, idx + m).map(_.toInt).toVector; idx += m
      lines += fmt(addLists(a, b))
    }
    print(lines.mkString("\n"))
  }
}

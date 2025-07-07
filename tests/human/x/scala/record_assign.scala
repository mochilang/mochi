case class Counter(var n: Int)

object record_assign {
  def inc(c: Counter): Unit = {
    c.n = c.n + 1
  }
  def main(args: Array[String]): Unit = {
    val c = Counter(0)
    inc(c)
    println(c.n)
  }
}

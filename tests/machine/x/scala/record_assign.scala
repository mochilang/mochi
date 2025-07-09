case class Counter(var n: Int)

object record_assign {
  def inc(c: Counter) = {
    c.n = c.n + 1
  }
  
  def main(args: Array[String]): Unit = {
    var c = Counter(n = 0)
    inc(c)
    println((c.n))
  }
}

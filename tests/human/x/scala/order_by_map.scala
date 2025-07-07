object order_by_map {
  case class Entry(a: Int, b: Int)

  def main(args: Array[String]): Unit = {
    val data = List(Entry(1,2), Entry(1,1), Entry(0,5))
    val sorted = data.sortBy(e => (e.a, e.b))
    val result = sorted.map(e => s"{\"a\":${e.a}, \"b\":${e.b}}")
    println("[" + result.mkString(", ") + "]")
  }
}


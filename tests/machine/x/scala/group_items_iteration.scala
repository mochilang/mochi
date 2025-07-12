object group_items_iteration {
  case class Auto1(tag: String, total: Int)
  case class Data(tag: String, `val`: Int)

  case class _Group[K,T](key: K, items: List[T]) extends Iterable[T] { def iterator: Iterator[T] = items.iterator }

  val data = List(Data(tag = "a", `val` = 1), Data(tag = "a", `val` = 2), Data(tag = "b", `val` = 3))
  val groups = ((for { d <- data } yield (d.tag, d)).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).map{ g => g }.toList
  def main(args: Array[String]): Unit = {
    var tmp = scala.collection.mutable.ArrayBuffer()
    for(g <- groups) {
      var total = 0
      for(x <- g.items) {
        total += x.`val`
      }
      tmp = tmp :+ Auto1(tag = g.key, total = total)
    }
    val result = (for { r <- tmp } yield r).sortBy(r => r.tag).map(r => r)
    println(result)
  }
}

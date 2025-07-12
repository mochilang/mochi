object group_by_having {
  case class Big(city: String, num: Int)
  case class People(name: String, city: String)

  case class _Group[K,T](key: K, items: List[T]) extends Iterable[T] { def iterator: Iterator[T] = items.iterator }

  val people = List(People(name = "Alice", city = "Paris"), People(name = "Bob", city = "Hanoi"), People(name = "Charlie", city = "Paris"), People(name = "Diana", city = "Hanoi"), People(name = "Eve", city = "Paris"), People(name = "Frank", city = "Hanoi"), People(name = "George", city = "Paris"))
  val big = (((for { p <- people } yield (p.city, p)).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).filter{ g => (g).size >= 4 }).map{ g => Map("city" -> (g.key), "num" -> ((g).size)) }.toList
  def main(args: Array[String]): Unit = {
    println(scala.util.parsing.json.JSONObject(big).toString())
  }
}

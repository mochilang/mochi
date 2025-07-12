object group_by {
  case class People(name: String, age: Int, city: String)
  case class Stat(city: String, count: Int, avg_age: Double)

  case class _Group[K,T](key: K, items: List[T]) extends Iterable[T] { def iterator: Iterator[T] = items.iterator }

  val people = List(People(name = "Alice", age = 30, city = "Paris"), People(name = "Bob", age = 15, city = "Hanoi"), People(name = "Charlie", age = 65, city = "Paris"), People(name = "Diana", age = 45, city = "Hanoi"), People(name = "Eve", age = 70, city = "Paris"), People(name = "Frank", age = 22, city = "Hanoi"))
  val stats = ((for { person <- people } yield (person.city, person)).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).map{ g => Stat(city = g.key, count = (g).size, avg_age = (for { p <- g } yield p.age).sum.toDouble / (for { p <- g } yield p.age).size) }.toList
  def main(args: Array[String]): Unit = {
    println("--- People grouped by city ---")
    for(s <- stats) {
      println(s.city + " " + ": count =" + " " + s.count + " " + ", avg_age =" + " " + s.avg_age)
    }
  }
}

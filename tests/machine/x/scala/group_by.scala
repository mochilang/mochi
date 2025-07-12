object group_by {
  case class People(name: String, age: Int, city: String)
  case class Stat(person: People)
  case class Stat1(city: String, count: Int, avg_age: Double)

  val people = List[People](People(name = "Alice", age = 30, city = "Paris"), People(name = "Bob", age = 15, city = "Hanoi"), People(name = "Charlie", age = 65, city = "Paris"), People(name = "Diana", age = 45, city = "Hanoi"), People(name = "Eve", age = 70, city = "Paris"), People(name = "Frank", age = 22, city = "Hanoi"))
  val stats = ((for { person <- people } yield (person.city, Stat(person = person))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Stat1(city = g.key, count = (g._2).size, avg_age = (for { p <- g._2 } yield p.age).sum.toDouble / (for { p <- g._2 } yield p.age).size) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- People grouped by city ---"))
    for(s <- stats) {
      println((s.city) + " " + (": count =") + " " + (s.count) + " " + (", avg_age =") + " " + (s.avg_age))
    }
  }
}

object group_by {
  case class Auto1(name: String, age: Int, city: String)
  case class Auto2(city: Any, count: Int, avg_age: Double)

  val people = List[Auto1](Auto1(name = "Alice", age = 30, city = "Paris"), Auto1(name = "Bob", age = 15, city = "Hanoi"), Auto1(name = "Charlie", age = 65, city = "Paris"), Auto1(name = "Diana", age = 45, city = "Hanoi"), Auto1(name = "Eve", age = 70, city = "Paris"), Auto1(name = "Frank", age = 22, city = "Hanoi"))
  val stats = ((for { person <- people } yield (person.city, (person))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Auto2(city = g._1, count = (g._2).size, avg_age = (for { p <- g._2 } yield p.age).sum.toDouble / (for { p <- g._2 } yield p.age).size) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- People grouped by city ---"))
    for(s <- stats) {
      println((s.city) + " " + (": count =") + " " + (s.count) + " " + (", avg_age =") + " " + (s.avg_age))
    }
  }
}

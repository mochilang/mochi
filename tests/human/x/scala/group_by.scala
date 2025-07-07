object group_by {
  case class Person(name: String, age: Int, city: String)
  case class Stats(city: String, count: Int, avgAge: Double)

  def main(args: Array[String]): Unit = {
    val people = List(
      Person("Alice", 30, "Paris"),
      Person("Bob", 15, "Hanoi"),
      Person("Charlie", 65, "Paris"),
      Person("Diana", 45, "Hanoi"),
      Person("Eve", 70, "Paris"),
      Person("Frank", 22, "Hanoi")
    )

    val stats = people.groupBy(_.city).map { case (city, ps) =>
      val ages = ps.map(_.age)
      Stats(city, ps.size, ages.sum.toDouble / ages.size)
    }.toList

    println("--- People grouped by city ---")
    stats.foreach { s =>
      println(s"${s.city}: count = ${s.count}, avg_age = ${s.avgAge}")
    }
  }
}

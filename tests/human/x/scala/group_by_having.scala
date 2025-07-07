object group_by_having {
  case class Person(name: String, city: String)
  case class Result(city: String, num: Int)

  def main(args: Array[String]): Unit = {
    val people = List(
      Person("Alice", "Paris"),
      Person("Bob", "Hanoi"),
      Person("Charlie", "Paris"),
      Person("Diana", "Hanoi"),
      Person("Eve", "Paris"),
      Person("Frank", "Hanoi"),
      Person("George", "Paris")
    )

    val big = people.groupBy(_.city).collect {
      case (city, list) if list.size >= 4 => Result(city, list.size)
    }

    big.foreach(r => println(s"{city: ${r.city}, num: ${r.num}}"))
  }
}

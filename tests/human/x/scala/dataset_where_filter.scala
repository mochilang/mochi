object dataset_where_filter {
  case class Person(name: String, age: Int)
  case class Adult(name: String, age: Int, isSenior: Boolean)

  def main(args: Array[String]): Unit = {
    val people = List(
      Person("Alice", 30),
      Person("Bob", 15),
      Person("Charlie", 65),
      Person("Diana", 45)
    )

    val adults = people.collect {
      case p if p.age >= 18 => Adult(p.name, p.age, p.age >= 60)
    }

    println("--- Adults ---")
    adults.foreach { person =>
      val note = if (person.isSenior) " (senior)" else ""
      println(s"${person.name} is ${person.age}$note")
    }
  }
}


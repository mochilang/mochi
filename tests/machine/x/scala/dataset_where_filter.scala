object dataset_where_filter {
  case class Adult(name: String, age: Int, is_senior: Boolean)
  case class People(name: String, age: Int)

  val people = List(People(name = "Alice", age = 30), People(name = "Bob", age = 15), People(name = "Charlie", age = 65), People(name = "Diana", age = 45))
  val adults = for { person <- people; if person.age >= 18 } yield Adult(name = person.name, age = person.age, is_senior = person.age >= 60)
  def main(args: Array[String]): Unit = {
    println("--- Adults ---")
    for(person <- adults) {
      println(person.name + " " + "is" + " " + person.age + " " + if (person.is_senior) " (senior)" else "")
    }
  }
}

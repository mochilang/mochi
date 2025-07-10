object dataset_where_filter {
  case class Auto1(name: String, age: Int)
  case class Auto2(name: String, age: Int, is_senior: Boolean)

  val people = List[Auto1](Auto1(name = "Alice", age = 30), Auto1(name = "Bob", age = 15), Auto1(name = "Charlie", age = 65), Auto1(name = "Diana", age = 45))
  val adults = for { person <- people; if person.age >= 18 } yield Auto2(name = person.name, age = person.age, is_senior = person.age >= 60)
  def main(args: Array[String]): Unit = {
    println(("--- Adults ---"))
    for(person <- adults) {
      println((person.name) + " " + ("is") + " " + (person.age) + " " + (if (person.is_senior) " (senior)" else ""))
    }
  }
}

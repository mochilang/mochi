object update_stmt {
  case class Person(var name: String, var age: Int, var status: String)

  def main(args: Array[String]): Unit = {
    val people = scala.collection.mutable.Buffer(
      Person("Alice", 17, "minor"),
      Person("Bob", 25, "unknown"),
      Person("Charlie", 18, "unknown"),
      Person("Diana", 16, "minor")
    )

    for(p <- people if p.age >= 18) {
      p.status = "adult"
      p.age = p.age + 1
    }

    val expected = Seq(
      Person("Alice", 17, "minor"),
      Person("Bob", 26, "adult"),
      Person("Charlie", 19, "adult"),
      Person("Diana", 16, "minor")
    )

    assert(people.toSeq == expected, "update adult status")
    println("ok")
  }
}


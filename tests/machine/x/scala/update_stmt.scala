case class Person(var name: String, var age: Int, var status: String)

object update_stmt {
  var people: scala.collection.mutable.ArrayBuffer[Person] = scala.collection.mutable.ArrayBuffer(Person(name = "Alice", age = 17, status = "minor"), Person(name = "Bob", age = 25, status = "unknown"), Person(name = "Charlie", age = 18, status = "unknown"), Person(name = "Diana", age = 16, status = "minor"))
  def main(args: Array[String]): Unit = {
    for(_i0 <- 0 until people.length) {
      var _it1 = people(_i0)
      var name = _it1.name
      var age = _it1.age
      var status = _it1.status
      if (age >= 18) {
        status = "adult"
        age = age + 1
      }
      _it1 = Person(name = name, age = age, status = status)
      people = people.updated(_i0, _it1)
    }
    assert(people == List(Person(name = "Alice", age = 17, status = "minor"), Person(name = "Bob", age = 26, status = "adult"), Person(name = "Charlie", age = 19, status = "adult"), Person(name = "Diana", age = 16, status = "minor")))
    println("ok")
  }
}

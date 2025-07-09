case class Person(var name: String, var age: Int)

case class Book(var title: String, var author: Person)

object user_type_literal {
  val book = Book(title = "Go", author = Person(name = "Bob", age = 42))
  def main(args: Array[String]): Unit = {
    println((book.author.name))
  }
}

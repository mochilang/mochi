case class Person(var name: String, var age: Int)

case class Book(var title: String, var author: Person)

object user_type_literal {
  def main(args: Array[String]): Unit = {
    val book = Book(title = "Go", author = Person(name = "Bob", age = 42))
    println((book.author.name))
  }
}

case class Person(name: String, age: Int)

case class Book(title: String, author: Person)

object user_type_literal {
  def main(args: Array[String]): Unit = {
    case class Person(name: String, age: Int)
    case class Book(title: String, author: Person)
    val book = Book(title = "Go", author = Person(name = "Bob", age = 42))
    println(book.author.name)
  }
}

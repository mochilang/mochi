case class Person(name: String, age: Int)
case class Book(title: String, author: Person)

object user_type_literal {
  def main(args: Array[String]): Unit = {
    val book = Book("Go", Person("Bob", 42))
    println(book.author.name)
  }
}

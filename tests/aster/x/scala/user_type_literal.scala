// Generated by Mochi v0.10.36 on 2025-07-22 17:43:25 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
object Main {
  case class Person(name: String, age: Int)
  case class Book(title: String, author: Person)
  val book: Book = Book("Go", Person("Bob", 42))
  def main(args: Array[String]): Unit = {
    println(book.author.name)
  }
}

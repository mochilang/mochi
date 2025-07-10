data class Person(var name: String, var age: Int)

data class Book(var title: String, var author: Person)

val book = Book(title = "Go", author = Person(name = "Bob", age = 42))

fun main() {
    println(book.author.name)
}

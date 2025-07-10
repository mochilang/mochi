data class Person(val name: String, val age: Int)

data class Book(val title: String, val author: Person)

val book = Book(title = "Go", author = Person(name = "Bob", age = 42))

fun main() {
    println(book.author.name)
}

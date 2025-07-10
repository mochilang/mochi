data class Person(val name: String, val age: Int)
data class Book(val title: String, val author: Person)

fun main() {
    val book = Book("Go", Person("Bob", 42))
    println(book.author.name)
}

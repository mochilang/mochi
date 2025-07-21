data class Person(var name: String, var age: Int)
data class Book(var title: String, var author: Person)
fun main() {
    val book: Book = Book(title = "Go", author = Person(name = "Bob", age = 42))
    println(book.author.name)
}

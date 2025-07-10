struct Person: Equatable {
    var name: String
    var age: Int
}
struct Book: Equatable {
    var title: String
    var author: Person
}
let book = Book(title: "Go", author: Person(name: "Bob", age: 42))
print(book.author.name)

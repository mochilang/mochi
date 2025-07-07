struct Person { var name: String; var age: Int; var email: String }

let people = [
    Person(name: "Alice", age: 30, email: "alice@example.com"),
    Person(name: "Bob", age: 15, email: "bob@example.com"),
    Person(name: "Charlie", age: 20, email: "charlie@example.com")
]

let adults = people.filter { $0.age >= 18 }.map { (name: $0.name, email: $0.email) }
for a in adults {
    print(a.name, a.email)
}

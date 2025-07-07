struct Person { var name: String; var age: Int; var status: String }
var people: [Person] = [
    Person(name: "Alice", age: 17, status: "minor"),
    Person(name: "Bob", age: 25, status: "unknown"),
    Person(name: "Charlie", age: 18, status: "unknown"),
    Person(name: "Diana", age: 16, status: "minor")
]
for i in 0..<people.count {
    if people[i].age >= 18 {
        people[i].status = "adult"
        people[i].age += 1
    }
}
assert(people == [
    Person(name: "Alice", age: 17, status: "minor"),
    Person(name: "Bob", age: 26, status: "adult"),
    Person(name: "Charlie", age: 19, status: "adult"),
    Person(name: "Diana", age: 16, status: "minor")
])
print("ok")

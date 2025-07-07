struct Person { let name: String; let age: Int }

let people = [
    Person(name: "Alice", age: 30),
    Person(name: "Bob", age: 15),
    Person(name: "Charlie", age: 65),
    Person(name: "Diana", age: 45)
]

let adults = people.filter { $0.age >= 18 }
                    .map { (name: $0.name, age: $0.age, isSenior: $0.age >= 60) }

print("--- Adults ---")
for person in adults {
    let label = person.isSenior ? " (senior)" : ""
    print("\(person.name) is \(person.age)\(label)")
}

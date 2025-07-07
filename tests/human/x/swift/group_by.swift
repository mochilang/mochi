struct Person { let name: String; let age: Int; let city: String }

let people = [
    Person(name: "Alice", age: 30, city: "Paris"),
    Person(name: "Bob", age: 15, city: "Hanoi"),
    Person(name: "Charlie", age: 65, city: "Paris"),
    Person(name: "Diana", age: 45, city: "Hanoi"),
    Person(name: "Eve", age: 70, city: "Paris"),
    Person(name: "Frank", age: 22, city: "Hanoi")
]

var groups: [String: [Person]] = [:]
for p in people {
    groups[p.city, default: []].append(p)
}

var stats: [(city: String, count: Int, avgAge: Double)] = []
for (city, persons) in groups {
    let total = persons.reduce(0) { $0 + $1.age }
    let avg = Double(total) / Double(persons.count)
    stats.append((city: city, count: persons.count, avgAge: avg))
}

print("--- People grouped by city ---")
for s in stats {
    print("\(s.city): count = \(s.count), avg_age = \(s.avgAge)")
}

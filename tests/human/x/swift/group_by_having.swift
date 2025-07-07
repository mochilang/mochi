struct Person { let name: String; let city: String }

let people = [
    Person(name: "Alice", city: "Paris"),
    Person(name: "Bob", city: "Hanoi"),
    Person(name: "Charlie", city: "Paris"),
    Person(name: "Diana", city: "Hanoi"),
    Person(name: "Eve", city: "Paris"),
    Person(name: "Frank", city: "Hanoi"),
    Person(name: "George", city: "Paris")
]

var groups: [String: [Person]] = [:]
for p in people {
    groups[p.city, default: []].append(p)
}

var big: [(city: String, num: Int)] = []
for (city, persons) in groups {
    if persons.count >= 4 {
        big.append((city: city, num: persons.count))
    }
}

print(big)

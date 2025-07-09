let people = [["name": "Alice", "age": 30], ["name": "Bob", "age": 15], ["name": "Charlie", "age": 65], ["name": "Diana", "age": 45]]
let adults = people.compactMap { person in person.age >= 18 ? ((name: person.name, age: person.age, is_senior: person.age >= 60)) : nil }
print("--- Adults ---")
for person in adults {
    print(person.name, "is", person.age, person.is_senior ? " (senior)" : "")
}

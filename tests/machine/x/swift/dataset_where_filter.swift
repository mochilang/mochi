struct AgeName: Equatable {
    var age: Int
    var name: String
}

var people = [AgeName(age: 30, name: "Alice"), AgeName(age: 15, name: "Bob"), AgeName(age: 65, name: "Charlie"), AgeName(age: 45, name: "Diana")]
var adults = people.compactMap { person in person.age >= 18 ? (["name": person.name, "age": person.age, "is_senior": person.age >= 60]) : nil }
print("--- Adults ---")
for person in adults as! [[String:Any]] {
    print(person["name"]!, "is", person["age"]!, person["is_senior"] as! Bool ? " (senior)" : "")
}

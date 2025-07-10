var people = [["name": "Alice", "age": 30], ["name": "Bob", "age": 15], ["name": "Charlie", "age": 65], ["name": "Diana", "age": 45]]
var adults = people.compactMap { person in person["age"] as! Int >= 18 ? (["name": person["name"] as! String, "age": person["age"] as! Int, "is_senior": person["age"] as! Int >= 60]) : nil }
print("--- Adults ---")
for person in adults {
    print(person["name"] as! String, "is", person["age"] as! Int, person["is_senior"] as! Bool ? " (senior)" : "")
}

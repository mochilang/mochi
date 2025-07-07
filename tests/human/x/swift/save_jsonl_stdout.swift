let people = [
    ["name": "Alice", "age": 30],
    ["name": "Bob", "age": 25]
]
for p in people {
    if let jsonData = try? JSONSerialization.data(withJSONObject: p),
       let jsonStr = String(data: jsonData, encoding: .utf8) {
        print(jsonStr)
    }
}

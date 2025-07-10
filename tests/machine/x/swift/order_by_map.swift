var data = [["a": 1, "b": 2], ["a": 1, "b": 1], ["a": 0, "b": 5]]
var sorted = data.map { x in (value: x, key: (a: x["a"] as! Int, b: x["b"] as! Int)) }.sorted { $0.key < $1.key }.map { $0.value }
print(sorted)

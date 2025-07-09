let data = [["a": 1, "b": 2], ["a": 1, "b": 1], ["a": 0, "b": 5]]
let sorted = data.map { x in (value: x, key: (a: x.a, b: x.b)) }.sorted { $0.key < $1.key }.map { $0.value }
print(sorted)

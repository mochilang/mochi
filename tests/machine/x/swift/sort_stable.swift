let items = [["n": 1, "v": "a"], ["n": 1, "v": "b"], ["n": 2, "v": "c"]]
let result = items.map { i in (value: i.v, key: i.n) }.sorted { $0.key < $1.key }.map { $0.value }
print(result)

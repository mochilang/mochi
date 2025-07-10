var items = [["n": 1, "v": "a"], ["n": 1, "v": "b"], ["n": 2, "v": "c"]]
var result = items.map { i in (value: i["v"] as! String, key: i["n"] as! Int) }.sorted { $0.key < $1.key }.map { $0.value }
print(result)

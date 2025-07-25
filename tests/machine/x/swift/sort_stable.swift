// Generated by Mochi compiler v0.10.27 on 2006-01-02T15:04:05Z
struct Item: Equatable {
    var n: Int
    var v: String
}

var items = [Item(n: 1, v: "a"), Item(n: 1, v: "b"), Item(n: 2, v: "c")]
var result = items.map { i in (value: i.v, key: i.n) }.sorted { $0.key < $1.key }.map { $0.value }
print(result)

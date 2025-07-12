struct NV: Equatable {
    var n: Int
    var v: String
}

var items = [NV(n: 1, v: "a"), NV(n: 1, v: "b"), NV(n: 2, v: "c")]
var result = items.map { i in (value: i.v, key: i.n) }.sorted { $0.key < $1.key }.map { $0.value }
print(result)

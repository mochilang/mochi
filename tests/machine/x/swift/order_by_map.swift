struct AB: Equatable {
    var a: Int
    var b: Int
}

var data = [AB(a: 1, b: 2), AB(a: 1, b: 1), AB(a: 0, b: 5)]
var sorted = data.map { x in (value: x, key: (a: x.a, b: x.b)) }.sorted { $0.key < $1.key }.map { $0.value }
print(sorted)

struct Auto1: Equatable {
    var a: Int
    var b: Int
}

var data = [Auto1(a: 1, b: 2), Auto1(a: 1, b: 1), Auto1(a: 0, b: 5)]
var sorted = data.map { x in (value: x, key: (a: x.a, b: x.b)) }.sorted { $0.key < $1.key }.map { $0.value }
print(sorted)

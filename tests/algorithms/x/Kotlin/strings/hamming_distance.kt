fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun hamming_distance(a: String, b: String): Int {
    if (a.length != b.length) {
        panic("String lengths must match!")
    }
    var i: Int = (0).toInt()
    var count: Int = (0).toInt()
    while (i < a.length) {
        if (a[i].toString() != b[i].toString()) {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

fun main() {
    println(hamming_distance("python", "python").toString())
    println(hamming_distance("karolin", "kathrin").toString())
    println(hamming_distance("00000", "11111").toString())
}

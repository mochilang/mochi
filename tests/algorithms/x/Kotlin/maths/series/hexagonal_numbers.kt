fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun hexagonal_numbers(length: Int): MutableList<Int> {
    if (length <= 0) {
        panic("Length must be a positive integer.")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var n: Int = (0).toInt()
    while (n < length) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(n * ((2 * n) - 1)); _tmp }
        n = n + 1
    }
    return res
}

fun test_hexagonal_numbers(): Unit {
    var expected5: MutableList<Int> = mutableListOf(0, 1, 6, 15, 28)
    var result5: MutableList<Int> = hexagonal_numbers(5)
    if (result5 != expected5) {
        panic("hexagonal_numbers(5) failed")
    }
    var expected10: MutableList<Int> = mutableListOf(0, 1, 6, 15, 28, 45, 66, 91, 120, 153)
    var result10: MutableList<Int> = hexagonal_numbers(10)
    if (result10 != expected10) {
        panic("hexagonal_numbers(10) failed")
    }
}

fun main() {
    test_hexagonal_numbers()
    println(hexagonal_numbers(5).toString())
    println(hexagonal_numbers(10).toString())
}

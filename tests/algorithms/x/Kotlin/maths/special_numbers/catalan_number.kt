fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun catalan(n: Int): Int {
    if (n < 1) {
        panic(("Input value of [number=" + _numToStr(n)) + "] must be > 0")
    }
    var current: Int = (1).toInt()
    var i: Int = (1).toInt()
    while (i < n) {
        current = current * ((4 * i) - 2)
        current = (current / (i + 1)).toInt()
        i = i + 1
    }
    return current
}

fun user_main(): Unit {
    if (catalan(1) != 1) {
        panic("catalan(1) should be 1")
    }
    if (catalan(5) != 14) {
        panic("catalan(5) should be 14")
    }
    println(_numToStr(catalan(5)))
}

fun main() {
    user_main()
}

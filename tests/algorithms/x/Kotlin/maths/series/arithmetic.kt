fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun is_arithmetic_series(xs: MutableList<Double>): Boolean {
    if (xs.size == 0) {
        panic("Input list must be a non empty list")
    }
    if (xs.size == 1) {
        return true
    }
    var diff: Double = xs[1]!! - xs[0]!!
    var i: Int = (0).toInt()
    while (i < (xs.size - 1)) {
        if ((xs[i + 1]!! - xs[i]!!) != diff) {
            return false
        }
        i = i + 1
    }
    return true
}

fun arithmetic_mean(xs: MutableList<Double>): Double {
    if (xs.size == 0) {
        panic("Input list must be a non empty list")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < xs.size) {
        total = total + xs[i]!!
        i = i + 1
    }
    return total / (xs.size.toDouble())
}

fun main() {
    println(is_arithmetic_series(mutableListOf(2.0, 4.0, 6.0)).toString())
    println(is_arithmetic_series(mutableListOf(3.0, 6.0, 12.0, 24.0)).toString())
    println(_numToStr(arithmetic_mean(mutableListOf(2.0, 4.0, 6.0))))
    println(_numToStr(arithmetic_mean(mutableListOf(3.0, 6.0, 9.0, 12.0))))
}

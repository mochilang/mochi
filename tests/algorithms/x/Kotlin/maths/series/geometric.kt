fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun is_geometric_series(series: MutableList<Double>): Boolean {
    if (series.size == 0) {
        panic("Input list must be a non empty list")
    }
    if (series.size == 1) {
        return true
    }
    if (series[0]!! == 0.0) {
        return false
    }
    var ratio: Double = series[1]!! / series[0]!!
    var i: Int = (0).toInt()
    while (i < (series.size - 1)) {
        if (series[i]!! == 0.0) {
            return false
        }
        if ((series[i + 1]!! / series[i]!!) != ratio) {
            return false
        }
        i = i + 1
    }
    return true
}

fun geometric_mean(series: MutableList<Double>): Double {
    if (series.size == 0) {
        panic("Input list must be a non empty list")
    }
    var product: Double = 1.0
    var i: Int = (0).toInt()
    while (i < series.size) {
        product = product * series[i]!!
        i = i + 1
    }
    var n: Int = (series.size).toInt()
    return nth_root(product, n)
}

fun pow_float(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun nth_root(value: Double, n: Int): Double {
    if (value == 0.0) {
        return 0.0
    }
    var low: Double = 0.0
    var high: Double = value
    if (value < 1.0) {
        high = 1.0
    }
    var mid: Double = (low + high) / 2.0
    var i: Int = (0).toInt()
    while (i < 40) {
        var mp: Double = pow_float(mid, n)
        if (mp > value) {
            high = mid
        } else {
            low = mid
        }
        mid = (low + high) / 2.0
        i = i + 1
    }
    return mid
}

fun test_geometric(): Unit {
    var a: MutableList<Double> = mutableListOf(2.0, 4.0, 8.0)
    if (!is_geometric_series(a)) {
        panic("expected geometric series")
    }
    var b: MutableList<Double> = mutableListOf(1.0, 2.0, 3.0)
    if ((is_geometric_series(b)) as Boolean) {
        panic("expected non geometric series")
    }
}

fun user_main(): Unit {
    test_geometric()
    println(geometric_mean(mutableListOf(2.0, 4.0, 8.0)))
}

fun main() {
    user_main()
}

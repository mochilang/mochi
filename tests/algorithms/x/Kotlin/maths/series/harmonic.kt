fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun is_harmonic_series(series: MutableList<Double>): Boolean {
    if (series.size == 0) {
        panic("Input list must be a non empty list")
    }
    if (series.size == 1) {
        if (series[0]!! == 0.0) {
            panic("Input series cannot have 0 as an element")
        }
        return true
    }
    var rec_series: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < series.size) {
        var _val: Double = series[i]!!
        if (_val == 0.0) {
            panic("Input series cannot have 0 as an element")
        }
        rec_series = run { val _tmp = rec_series.toMutableList(); _tmp.add(1.0 / _val); _tmp }
        i = i + 1
    }
    var common_diff: Double = rec_series[1]!! - rec_series[0]!!
    var idx: Int = (2).toInt()
    while (idx < rec_series.size) {
        if ((rec_series[idx]!! - rec_series[idx - 1]!!) != common_diff) {
            return false
        }
        idx = idx + 1
    }
    return true
}

fun harmonic_mean(series: MutableList<Double>): Double {
    if (series.size == 0) {
        panic("Input list must be a non empty list")
    }
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < series.size) {
        total = total + (1.0 / series[i]!!)
        i = i + 1
    }
    return (series.size.toDouble()) / total
}

fun main() {
    println(is_harmonic_series(mutableListOf(1.0, 2.0 / 3.0, 1.0 / 2.0, 2.0 / 5.0, 1.0 / 3.0)))
    println(is_harmonic_series(mutableListOf(1.0, 2.0 / 3.0, 2.0 / 5.0, 1.0 / 3.0)))
    println(harmonic_mean(mutableListOf(1.0, 4.0, 4.0)))
    println(harmonic_mean(mutableListOf(3.0, 6.0, 9.0, 12.0)))
}

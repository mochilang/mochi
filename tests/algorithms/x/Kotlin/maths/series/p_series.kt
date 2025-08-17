fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun pow_string(base: Int, exp: Int): String {
    if (exp >= 0) {
        var res: Int = (1).toInt()
        var i: Int = (0).toInt()
        while (i < exp) {
            res = res * base
            i = i + 1
        }
        return _numToStr(res)
    }
    var e: Int = (0 - exp).toInt()
    var res: Double = 1.0
    var b: Double = (base).toDouble() * 1.0
    var i: Int = (0).toInt()
    while (i < e) {
        res = res * b
        i = i + 1
    }
    var value: Double = 1.0 / res
    return _numToStr(value)
}

fun p_series(nth_term: Int, power: Int): MutableList<String> {
    var series: MutableList<String> = mutableListOf<String>()
    if (nth_term <= 0) {
        return series
    }
    var i: Int = (1).toInt()
    while (i <= nth_term) {
        if (i == 1) {
            series = run { val _tmp = series.toMutableList(); _tmp.add("1"); _tmp }
        } else {
            series = run { val _tmp = series.toMutableList(); _tmp.add("1 / " + pow_string(i, power)); _tmp }
        }
        i = i + 1
    }
    return series
}

fun main() {
    println(p_series(5, 2))
    println(p_series(0 - 5, 2))
    println(p_series(5, 0 - 2))
    println(p_series(0, 0))
    println(p_series(1, 1))
}

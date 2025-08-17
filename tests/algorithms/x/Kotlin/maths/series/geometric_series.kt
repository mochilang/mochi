fun geometric_series(nth_term: Double, start_term_a: Double, common_ratio_r: Double): MutableList<Double> {
    var n: Int = (nth_term.toInt()).toInt()
    if ((((n <= 0) || (start_term_a == 0.0) as Boolean)) || (common_ratio_r == 0.0)) {
        return mutableListOf<Double>()
    }
    var series: MutableList<Double> = mutableListOf<Double>()
    var current: Double = start_term_a
    var i: Int = (0).toInt()
    while (i < n) {
        series = run { val _tmp = series.toMutableList(); _tmp.add(current); _tmp }
        current = current * common_ratio_r
        i = i + 1
    }
    return series
}

fun main() {
    println(geometric_series(4.0, 2.0, 2.0))
    println(geometric_series(4.0, 2.0, 0.0 - 2.0))
    println(geometric_series(4.0, 0.0 - 2.0, 2.0))
    println(geometric_series(0.0 - 4.0, 2.0, 2.0))
    println(geometric_series(0.0, 100.0, 500.0))
    println(geometric_series(1.0, 1.0, 1.0))
}

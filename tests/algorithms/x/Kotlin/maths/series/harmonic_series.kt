fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun harmonic_series(n_term: Double): MutableList<String> {
    if (n_term <= 0.0) {
        return mutableListOf<String>()
    }
    var limit: Int = (n_term.toInt()).toInt()
    var series: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < limit) {
        if (i == 0) {
            series = run { val _tmp = series.toMutableList(); _tmp.add("1"); _tmp }
        } else {
            series = run { val _tmp = series.toMutableList(); _tmp.add("1/" + _numToStr(i + 1)); _tmp }
        }
        i = i + 1
    }
    return series
}

fun main() {
    println(harmonic_series(5.0).toString())
}

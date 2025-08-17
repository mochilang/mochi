fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun assign_ranks(data: MutableList<Double>): MutableList<Int> {
    var ranks: MutableList<Int> = mutableListOf<Int>()
    var n: Int = (data.size).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        var rank: Int = (1).toInt()
        var j: Int = (0).toInt()
        while (j < n) {
            if ((data[j]!! < data[i]!!) || (((data[j]!! == data[i]!!) && (j < i) as Boolean))) {
                rank = rank + 1
            }
            j = j + 1
        }
        ranks = run { val _tmp = ranks.toMutableList(); _tmp.add(rank); _tmp }
        i = i + 1
    }
    return ranks
}

fun calculate_spearman_rank_correlation(var1: MutableList<Double>, var2: MutableList<Double>): Double {
    if (var1.size != var2.size) {
        panic("Lists must have equal length")
    }
    var n: Int = (var1.size).toInt()
    var rank1: MutableList<Int> = assign_ranks(var1)
    var rank2: MutableList<Int> = assign_ranks(var2)
    var i: Int = (0).toInt()
    var d_sq: Double = 0.0
    while (i < n) {
        var diff: Double = (rank1[i]!! - rank2[i]!!).toDouble()
        d_sq = d_sq + (diff * diff)
        i = i + 1
    }
    var n_f: Double = n.toDouble()
    return 1.0 - ((6.0 * d_sq) / (n_f * ((n_f * n_f) - 1.0)))
}

fun test_spearman(): Unit {
    var x: MutableList<Double> = mutableListOf(1.0, 2.0, 3.0, 4.0, 5.0)
    var y_inc: MutableList<Double> = mutableListOf(2.0, 4.0, 6.0, 8.0, 10.0)
    if (calculate_spearman_rank_correlation(x, y_inc) != 1.0) {
        panic("case1")
    }
    var y_dec: MutableList<Double> = mutableListOf(5.0, 4.0, 3.0, 2.0, 1.0)
    if (calculate_spearman_rank_correlation(x, y_dec) != (0.0 - 1.0)) {
        panic("case2")
    }
    var y_mix: MutableList<Double> = mutableListOf(5.0, 1.0, 2.0, 9.0, 5.0)
    if (calculate_spearman_rank_correlation(x, y_mix) != 0.6) {
        panic("case3")
    }
}

fun user_main(): Unit {
    test_spearman()
    println(_numToStr(calculate_spearman_rank_correlation(mutableListOf(1.0, 2.0, 3.0, 4.0, 5.0), mutableListOf(2.0, 4.0, 6.0, 8.0, 10.0))))
    println(_numToStr(calculate_spearman_rank_correlation(mutableListOf(1.0, 2.0, 3.0, 4.0, 5.0), mutableListOf(5.0, 4.0, 3.0, 2.0, 1.0))))
    println(_numToStr(calculate_spearman_rank_correlation(mutableListOf(1.0, 2.0, 3.0, 4.0, 5.0), mutableListOf(5.0, 1.0, 2.0, 9.0, 5.0))))
}

fun main() {
    user_main()
}

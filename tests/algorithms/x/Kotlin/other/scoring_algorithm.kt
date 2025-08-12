fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

var vehicles: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
fun get_data(source_data: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var data_lists: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < source_data.size) {
        var row: MutableList<Double> = source_data[i]!!
        var j: Int = (0).toInt()
        while (j < row.size) {
            if (data_lists.size < (j + 1)) {
                var empty: MutableList<Double> = mutableListOf<Double>()
                data_lists = run { val _tmp = data_lists.toMutableList(); _tmp.add(empty); _tmp }
            }
            _listSet(data_lists, j, run { val _tmp = (data_lists[j]!!).toMutableList(); _tmp.add(row[j]!!); _tmp })
            j = j + 1
        }
        i = i + 1
    }
    return data_lists
}

fun calculate_each_score(data_lists: MutableList<MutableList<Double>>, weights: MutableList<Int>): MutableList<MutableList<Double>> {
    var score_lists: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < data_lists.size) {
        var dlist: MutableList<Double> = data_lists[i]!!
        var weight: Int = (weights[i]!!).toInt()
        var mind: Double = dlist[0]!!
        var maxd: Double = dlist[0]!!
        var j: Int = (1).toInt()
        while (j < dlist.size) {
            var _val: Double = dlist[j]!!
            if (_val < mind) {
                mind = _val
            }
            if (_val > maxd) {
                maxd = _val
            }
            j = j + 1
        }
        var score: MutableList<Double> = mutableListOf<Double>()
        j = 0
        if (weight == 0) {
            while (j < dlist.size) {
                var item: Double = dlist[j]!!
                if ((maxd - mind) == 0.0) {
                    score = run { val _tmp = score.toMutableList(); _tmp.add(1.0); _tmp }
                } else {
                    score = run { val _tmp = score.toMutableList(); _tmp.add(1.0 - ((item - mind) / (maxd - mind))); _tmp }
                }
                j = j + 1
            }
        } else {
            while (j < dlist.size) {
                var item: Double = dlist[j]!!
                if ((maxd - mind) == 0.0) {
                    score = run { val _tmp = score.toMutableList(); _tmp.add(0.0); _tmp }
                } else {
                    score = run { val _tmp = score.toMutableList(); _tmp.add((item - mind) / (maxd - mind)); _tmp }
                }
                j = j + 1
            }
        }
        score_lists = run { val _tmp = score_lists.toMutableList(); _tmp.add(score); _tmp }
        i = i + 1
    }
    return score_lists
}

fun generate_final_scores(score_lists: MutableList<MutableList<Double>>): MutableList<Double> {
    var count: Int = ((score_lists[0]!!).size).toInt()
    var final_scores: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < count) {
        final_scores = run { val _tmp = final_scores.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    i = 0
    while (i < score_lists.size) {
        var slist: MutableList<Double> = score_lists[i]!!
        var j: Int = (0).toInt()
        while (j < slist.size) {
            _listSet(final_scores, j, final_scores[j]!! + slist[j]!!)
            j = j + 1
        }
        i = i + 1
    }
    return final_scores
}

fun procentual_proximity(source_data: MutableList<MutableList<Double>>, weights: MutableList<Int>): MutableList<MutableList<Double>> {
    var data_lists: MutableList<MutableList<Double>> = get_data(source_data)
    var score_lists: MutableList<MutableList<Double>> = calculate_each_score(data_lists, weights)
    var final_scores: MutableList<Double> = generate_final_scores(score_lists)
    var i: Int = (0).toInt()
    while (i < final_scores.size) {
        _listSet(source_data, i, run { val _tmp = (source_data[i]!!).toMutableList(); _tmp.add(final_scores[i]!!); _tmp })
        i = i + 1
    }
    return source_data
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        vehicles = run { val _tmp = vehicles.toMutableList(); _tmp.add(mutableListOf(20.0, 60.0, 2012.0)); _tmp }
        vehicles = run { val _tmp = vehicles.toMutableList(); _tmp.add(mutableListOf(23.0, 90.0, 2015.0)); _tmp }
        vehicles = run { val _tmp = vehicles.toMutableList(); _tmp.add(mutableListOf(22.0, 50.0, 2011.0)); _tmp }
        var weights: MutableList<Int> = mutableListOf(0, 0, 1)
        var result: MutableList<MutableList<Double>> = procentual_proximity(vehicles, weights)
        println(result.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

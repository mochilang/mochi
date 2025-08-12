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

data class PointLabel(var point: MutableList<Double> = mutableListOf<Double>(), var label: Int = 0)
data class KNN(var data: MutableList<PointLabel> = mutableListOf<PointLabel>(), var labels: MutableList<String> = mutableListOf<String>())
data class DistLabel(var dist: Double = 0.0, var label: Int = 0)
var train_X: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0), mutableListOf(1.0, 0.0), mutableListOf(0.0, 1.0), mutableListOf(0.5, 0.5), mutableListOf(3.0, 3.0), mutableListOf(2.0, 3.0), mutableListOf(3.0, 2.0))
var train_y: MutableList<Int> = mutableListOf(0, 0, 0, 0, 1, 1, 1)
var classes: MutableList<String> = mutableListOf("A", "B")
var knn: KNN = make_knn(train_X, train_y, classes)
var point: MutableList<Double> = mutableListOf(1.2, 1.2)
fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun make_knn(train_data: MutableList<MutableList<Double>>, train_target: MutableList<Int>, class_labels: MutableList<String>): KNN {
    var items: MutableList<PointLabel> = mutableListOf<PointLabel>()
    var i: Int = (0).toInt()
    while (i < train_data.size) {
        var pl: PointLabel = PointLabel(point = train_data[i]!!, label = train_target[i]!!)
        items = run { val _tmp = items.toMutableList(); _tmp.add(pl); _tmp }
        i = i + 1
    }
    return KNN(data = items, labels = class_labels)
}

fun euclidean_distance(a: MutableList<Double>, b: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        var diff: Double = a[i]!! - b[i]!!
        sum = sum + (diff * diff)
        i = i + 1
    }
    return sqrtApprox(sum)
}

fun classify(knn: KNN, pred_point: MutableList<Double>, k: Int): String {
    var distances: MutableList<DistLabel> = mutableListOf<DistLabel>()
    var i: Int = (0).toInt()
    while (i < (knn.data).size) {
        var d: Double = euclidean_distance((((knn.data)[i]!!.point) as MutableList<Double>), pred_point)
        distances = run { val _tmp = distances.toMutableList(); _tmp.add(DistLabel(dist = d, label = (knn.data)[i]!!.label)); _tmp }
        i = i + 1
    }
    var votes: MutableList<Int> = mutableListOf<Int>()
    var count: Int = (0).toInt()
    while (count < k) {
        var min_index: Int = (0).toInt()
        var j: Int = (1).toInt()
        while (j < distances.size) {
            if (distances[j]!!.dist < distances[min_index]!!.dist) {
                min_index = j
            }
            j = j + 1
        }
        votes = run { val _tmp = votes.toMutableList(); _tmp.add(((distances[min_index]!!.label) as Int)); _tmp }
        distances[min_index]!!.dist = 1000000000000000000.0
        count = count + 1
    }
    var tally: MutableList<Int> = mutableListOf<Int>()
    var t: Int = (0).toInt()
    while (t < (knn.labels).size) {
        tally = run { val _tmp = tally.toMutableList(); _tmp.add(0); _tmp }
        t = t + 1
    }
    var v: Int = (0).toInt()
    while (v < votes.size) {
        var lbl: Int = (votes[v]!!).toInt()
        _listSet(tally, lbl, tally[lbl]!! + 1)
        v = v + 1
    }
    var max_idx: Int = (0).toInt()
    var m: Int = (1).toInt()
    while (m < tally.size) {
        if (tally[m]!! > tally[max_idx]!!) {
            max_idx = m
        }
        m = m + 1
    }
    return (knn.labels)[max_idx]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(classify(knn, point, 5))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

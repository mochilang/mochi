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

data class KMeansResult(var centroids: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var assignments: MutableList<Int> = mutableListOf<Int>(), var heterogeneity: MutableList<Double> = mutableListOf<Double>())
var data: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 2.0), mutableListOf(1.5, 1.8), mutableListOf(5.0, 8.0), mutableListOf(8.0, 8.0), mutableListOf(1.0, 0.6), mutableListOf(9.0, 11.0))
var k: Int = (3).toInt()
var initial_centroids: MutableList<MutableList<Double>> = mutableListOf(data[0]!!, data[2]!!, data[5]!!)
var result: KMeansResult = kmeans(data, k, initial_centroids, 10)
fun distance_sq(a: MutableList<Double>, b: MutableList<Double>): Double {
    var sum: Double = 0.0
    for (i in 0 until a.size) {
        var diff: Double = a[i]!! - b[i]!!
        sum = sum + (diff * diff)
    }
    return sum
}

fun assign_clusters(data: MutableList<MutableList<Double>>, centroids: MutableList<MutableList<Double>>): MutableList<Int> {
    var assignments: MutableList<Int> = mutableListOf<Int>()
    for (i in 0 until data.size) {
        var best_idx: Int = (0).toInt()
        var best: Double = distance_sq(data[i]!!, centroids[0]!!)
        for (j in 1 until centroids.size) {
            var dist: Double = distance_sq(data[i]!!, centroids[j]!!)
            if (dist < best) {
                best = dist
                best_idx = j
            }
        }
        assignments = run { val _tmp = assignments.toMutableList(); _tmp.add(best_idx); _tmp }
    }
    return assignments
}

fun revise_centroids(data: MutableList<MutableList<Double>>, k: Int, assignment: MutableList<Int>): MutableList<MutableList<Double>> {
    var dim: Int = ((data[0]!!).size).toInt()
    var sums: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var counts: MutableList<Int> = mutableListOf<Int>()
    for (i in 0 until k) {
        var row: MutableList<Double> = mutableListOf<Double>()
        for (j in 0 until dim) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
        }
        sums = run { val _tmp = sums.toMutableList(); _tmp.add(row); _tmp }
        counts = run { val _tmp = counts.toMutableList(); _tmp.add(0); _tmp }
    }
    for (i in 0 until data.size) {
        var c: Int = (assignment[i]!!).toInt()
        _listSet(counts, c, counts[c]!! + 1)
        for (j in 0 until dim) {
            _listSet(sums[c]!!, j, (((sums[c]!!) as MutableList<Double>))[j]!! + (((data[i]!!) as MutableList<Double>))[j]!!)
        }
    }
    var centroids: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    for (i in 0 until k) {
        var row: MutableList<Double> = mutableListOf<Double>()
        if (counts[i]!! > 0) {
            for (j in 0 until dim) {
                row = run { val _tmp = row.toMutableList(); _tmp.add((((sums[i]!!) as MutableList<Double>))[j]!! / (((counts[i]!!).toDouble()))); _tmp }
            }
        } else {
            for (j in 0 until dim) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            }
        }
        centroids = run { val _tmp = centroids.toMutableList(); _tmp.add(row); _tmp }
    }
    return centroids
}

fun compute_heterogeneity(data: MutableList<MutableList<Double>>, centroids: MutableList<MutableList<Double>>, assignment: MutableList<Int>): Double {
    var total: Double = 0.0
    for (i in 0 until data.size) {
        var c: Int = (assignment[i]!!).toInt()
        total = total + distance_sq(data[i]!!, centroids[c]!!)
    }
    return total
}

fun lists_equal(a: MutableList<Int>, b: MutableList<Int>): Boolean {
    if (a.size != b.size) {
        return false
    }
    for (i in 0 until a.size) {
        if (a[i]!! != b[i]!!) {
            return false
        }
    }
    return true
}

fun kmeans(data: MutableList<MutableList<Double>>, k: Int, initial_centroids: MutableList<MutableList<Double>>, max_iter: Int): KMeansResult {
    var centroids: MutableList<MutableList<Double>> = initial_centroids
    var assignment: MutableList<Int> = mutableListOf<Int>()
    var prev: MutableList<Int> = mutableListOf<Int>()
    var heterogeneity: MutableList<Double> = mutableListOf<Double>()
    var iter: Int = (0).toInt()
    while (iter < max_iter) {
        assignment = assign_clusters(data, centroids)
        centroids = revise_centroids(data, k, assignment)
        var h: Double = compute_heterogeneity(data, centroids, assignment)
        heterogeneity = run { val _tmp = heterogeneity.toMutableList(); _tmp.add(h); _tmp }
        if ((iter > 0) && lists_equal(prev, assignment)) {
            break
        }
        prev = assignment
        iter = iter + 1
    }
    return KMeansResult(centroids = centroids, assignments = assignment, heterogeneity = heterogeneity)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(result.centroids.toString())
        println(result.assignments.toString())
        println(result.heterogeneity.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

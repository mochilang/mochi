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

data class KMeansResult(var centroids: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var assignments: MutableList<Int> = mutableListOf<Int>())
fun distance_sq(a: MutableList<Double>, b: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        var diff: Double = a[i]!! - b[i]!!
        sum = sum + (diff * diff)
        i = i + 1
    }
    return sum
}

fun mean(vectors: MutableList<MutableList<Double>>): MutableList<Double> {
    var dim: Int = ((vectors[0]!!).size).toInt()
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < dim) {
        var total: Double = 0.0
        var j: Int = (0).toInt()
        while (j < vectors.size) {
            total = total + (((vectors[j]!!) as MutableList<Double>))[i]!!
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(total / vectors.size); _tmp }
        i = i + 1
    }
    return res
}

fun k_means(vectors: MutableList<MutableList<Double>>, k: Int, iterations: Int): KMeansResult {
    var centroids: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < k) {
        centroids = run { val _tmp = centroids.toMutableList(); _tmp.add(vectors[i]!!); _tmp }
        i = i + 1
    }
    var assignments: MutableList<Int> = mutableListOf<Int>()
    var n: Int = (vectors.size).toInt()
    i = 0
    while (i < n) {
        assignments = run { val _tmp = assignments.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var it: Int = (0).toInt()
    while (it < iterations) {
        var v: Int = (0).toInt()
        while (v < n) {
            var best: Int = (0).toInt()
            var bestDist: Double = distance_sq(vectors[v]!!, centroids[0]!!)
            var c: Int = (1).toInt()
            while (c < k) {
                var d: Double = distance_sq(vectors[v]!!, centroids[c]!!)
                if (d < bestDist) {
                    bestDist = d
                    best = c
                }
                c = c + 1
            }
            _listSet(assignments, v, best)
            v = v + 1
        }
        var cIdx: Int = (0).toInt()
        while (cIdx < k) {
            var cluster: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
            var v2: Int = (0).toInt()
            while (v2 < n) {
                if (assignments[v2]!! == cIdx) {
                    cluster = run { val _tmp = cluster.toMutableList(); _tmp.add(vectors[v2]!!); _tmp }
                }
                v2 = v2 + 1
            }
            if (cluster.size > 0) {
                _listSet(centroids, cIdx, mean(cluster))
            }
            cIdx = cIdx + 1
        }
        it = it + 1
    }
    return KMeansResult(centroids = centroids, assignments = assignments)
}

fun user_main(): Unit {
    var vectors: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 2.0), mutableListOf(1.5, 1.8), mutableListOf(5.0, 8.0), mutableListOf(8.0, 8.0), mutableListOf(1.0, 0.6), mutableListOf(9.0, 11.0))
    var result: KMeansResult = k_means(vectors, 2, 5)
    println(result.centroids.toString())
    println(result.assignments.toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

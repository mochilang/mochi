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

data class Neighbor(var vector: MutableList<Double> = mutableListOf<Double>(), var distance: Double = 0.0)
var dataset: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0, 0.0), mutableListOf(1.0, 1.0, 1.0), mutableListOf(2.0, 2.0, 2.0))
var value_array: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 1.0))
var neighbors: MutableList<Neighbor> = similarity_search(dataset, value_array)
var k: Int = (0).toInt()
fun sqrt(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun euclidean(a: MutableList<Double>, b: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        var diff: Double = a[i]!! - b[i]!!
        sum = sum + (diff * diff)
        i = i + 1
    }
    var res: Double = sqrt(sum)
    return res
}

fun similarity_search(dataset: MutableList<MutableList<Double>>, value_array: MutableList<MutableList<Double>>): MutableList<Neighbor> {
    var dim: Int = ((dataset[0]!!).size).toInt()
    if (dim != (value_array[0]!!).size) {
        return mutableListOf<Neighbor>()
    }
    var result: MutableList<Neighbor> = mutableListOf<Neighbor>()
    var i: Int = (0).toInt()
    while (i < value_array.size) {
        var value: MutableList<Double> = value_array[i]!!
        var dist: Double = euclidean(value, dataset[0]!!)
        var vec: MutableList<Double> = dataset[0]!!
        var j: Int = (1).toInt()
        while (j < dataset.size) {
            var d: Double = euclidean(value, dataset[j]!!)
            if (d < dist) {
                dist = d
                vec = dataset[j]!!
            }
            j = j + 1
        }
        var nb: Neighbor = Neighbor(vector = vec, distance = dist)
        result = run { val _tmp = result.toMutableList(); _tmp.add(nb); _tmp }
        i = i + 1
    }
    return result
}

fun cosine_similarity(a: MutableList<Double>, b: MutableList<Double>): Double {
    var dot: Double = 0.0
    var norm_a: Double = 0.0
    var norm_b: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        dot = dot + (a[i]!! * b[i]!!)
        norm_a = norm_a + (a[i]!! * a[i]!!)
        norm_b = norm_b + (b[i]!! * b[i]!!)
        i = i + 1
    }
    if ((norm_a == 0.0) || (norm_b == 0.0)) {
        return 0.0
    }
    return dot / (sqrt(norm_a) * sqrt(norm_b))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (k < neighbors.size) {
            var n: Neighbor = neighbors[k]!!
            println(((("[" + n.vector.toString()) + ", ") + n.distance.toString()) + "]")
            k = (k + 1).toInt()
        }
        println(cosine_similarity(mutableListOf(1.0, 2.0), mutableListOf(6.0, 32.0)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

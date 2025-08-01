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

fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun cholesky(a: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var n: Int = a.size
    var l: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = 0
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = 0
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp } as MutableList<Double>
            j = j + 1
        }
        l = run { val _tmp = l.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Double>>
        i = i + 1
    }
    i = 0
    while (i < n) {
        var j: Int = 0
        while (j <= i) {
            var sum: Double = ((a[i]!!) as MutableList<Double>)[j]!!
            var k: Int = 0
            while (k < j) {
                sum = sum - (((l[i]!!) as MutableList<Double>)[k]!! * ((l[j]!!) as MutableList<Double>)[k]!!)
                k = k + 1
            }
            if (i == j) {
                ((l[i]!!)[j]) = sqrtApprox(sum)
            } else {
                ((l[i]!!)[j]) = sum / ((l[j]!!) as MutableList<Double>)[j]!!
            }
            j = j + 1
        }
        i = i + 1
    }
    return l
}

fun printMat(m: MutableList<MutableList<Double>>): Unit {
    var i: Int = 0
    while (i < m.size) {
        var line: String = ""
        var j: Int = 0
        while (j < (m[i]!!).size) {
            line = line + (((m[i]!!) as MutableList<Double>)[j]!!).toString()
            if (j < ((m[i]!!).size - 1)) {
                line = line + " "
            }
            j = j + 1
        }
        println(line)
        i = i + 1
    }
}

fun demo(a: MutableList<MutableList<Double>>): Unit {
    println("A:")
    printMat(a)
    var l: MutableList<MutableList<Double>> = cholesky(a)
    println("L:")
    printMat(l)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        demo(mutableListOf(mutableListOf(25.0, 15.0, 0.0 - 5.0), mutableListOf(15.0, 18.0, 0.0), mutableListOf(0.0 - 5.0, 0.0, 11.0)))
        demo(mutableListOf(mutableListOf(18.0, 22.0, 54.0, 42.0), mutableListOf(22.0, 70.0, 86.0, 62.0), mutableListOf(54.0, 86.0, 174.0, 134.0), mutableListOf(42.0, 62.0, 134.0, 106.0)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

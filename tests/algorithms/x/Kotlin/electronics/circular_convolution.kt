import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/electronics"

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

var example1: MutableList<Double> = circular_convolution(mutableListOf(2.0, 1.0, 2.0, 0.0 - 1.0), mutableListOf(1.0, 2.0, 3.0, 4.0))
fun floor(x: Double): Double {
    var i: Int = (x.toInt()).toInt()
    if ((i.toDouble()) > x) {
        i = i - 1
    }
    return i.toDouble()
}

fun pow10(n: Int): Double {
    var p: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        p = p * 10.0
        i = i + 1
    }
    return p
}

fun roundn(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    return floor((x * m) + 0.5) / m
}

fun pad(signal: MutableList<Double>, target: Int): MutableList<Double> {
    var s: MutableList<Double> = signal
    while (s.size < target) {
        s = run { val _tmp = s.toMutableList(); _tmp.add(0.0); _tmp }
    }
    return s
}

fun circular_convolution(a: MutableList<Double>, b: MutableList<Double>): MutableList<Double> {
    var n1: Int = (a.size).toInt()
    var n2: Int = (b.size).toInt()
    var n: Int = (if (n1 > n2) n1 else n2).toInt()
    var x: MutableList<Double> = pad(a, n)
    var y: MutableList<Double> = pad(b, n)
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < n) {
        var sum: Double = 0.0
        var k: Int = (0).toInt()
        while (k < n) {
            var j: Int = (Math.floorMod((i - k), n)).toInt()
            var idx = if (j < 0) j + n else j
            sum = sum + (x[k]!! * y[(idx).toInt()]!!)
            k = k + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(roundn(sum, 2)); _tmp }
        i = i + 1
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(example1.toString())
        var example2: MutableList<Double> = circular_convolution(mutableListOf(0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6), mutableListOf(0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5))
        println(example2.toString())
        var example3: MutableList<Double> = circular_convolution(mutableListOf(0.0 - 1.0, 1.0, 2.0, 0.0 - 2.0), mutableListOf(0.5, 1.0, 0.0 - 1.0, 2.0, 0.75))
        println(example3.toString())
        var example4: MutableList<Double> = circular_convolution(mutableListOf(1.0, 0.0 - 1.0, 2.0, 3.0, 0.0 - 1.0), mutableListOf(1.0, 2.0, 3.0))
        println(example4.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

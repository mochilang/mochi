val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/electronics"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

var COULOMBS_CONSTANT: Double = 8988000000.0
fun abs(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

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

fun coulombs_law(force: Double, charge1: Double, charge2: Double, distance: Double): MutableMap<String, Double> {
    var charge_product: Double = abs(charge1 * charge2)
    var zero_count: Int = (0).toInt()
    if (force == 0.0) {
        zero_count = zero_count + 1
    }
    if (charge1 == 0.0) {
        zero_count = zero_count + 1
    }
    if (charge2 == 0.0) {
        zero_count = zero_count + 1
    }
    if (distance == 0.0) {
        zero_count = zero_count + 1
    }
    if (zero_count != 1) {
        panic("One and only one argument must be 0")
    }
    if (distance < 0.0) {
        panic("Distance cannot be negative")
    }
    if (force == 0.0) {
        var f: Double = (COULOMBS_CONSTANT * charge_product) / (distance * distance)
        return mutableMapOf<String, Double>("force" to (f))
    }
    if (charge1 == 0.0) {
        var c1: Double = (abs(force) * (distance * distance)) / (COULOMBS_CONSTANT * charge2)
        return mutableMapOf<String, Double>("charge1" to (c1))
    }
    if (charge2 == 0.0) {
        var c2: Double = (abs(force) * (distance * distance)) / (COULOMBS_CONSTANT * charge1)
        return mutableMapOf<String, Double>("charge2" to (c2))
    }
    var d: Double = sqrtApprox((COULOMBS_CONSTANT * charge_product) / abs(force))
    return mutableMapOf<String, Double>("distance" to (d))
}

fun print_map(m: MutableMap<String, Double>): Unit {
    for (k in m.keys) {
        println(((("{\"" + k) + "\": ") + _numToStr((m)[k] as Double)) + "}")
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        print_map(coulombs_law(0.0, 3.0, 5.0, 2000.0))
        print_map(coulombs_law(10.0, 3.0, 5.0, 0.0))
        print_map(coulombs_law(10.0, 0.0, 5.0, 2000.0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

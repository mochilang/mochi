val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/electronics"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

var PI: Double = 3.141592653589793
fun ind_reactance(inductance: Double, frequency: Double, reactance: Double): MutableMap<String, Double> {
    var zero_count: Int = (0).toInt()
    if (inductance == 0.0) {
        zero_count = zero_count + 1
    }
    if (frequency == 0.0) {
        zero_count = zero_count + 1
    }
    if (reactance == 0.0) {
        zero_count = zero_count + 1
    }
    if (zero_count != 1) {
        panic("One and only one argument must be 0")
    }
    if (inductance < 0.0) {
        panic("Inductance cannot be negative")
    }
    if (frequency < 0.0) {
        panic("Frequency cannot be negative")
    }
    if (reactance < 0.0) {
        panic("Inductive reactance cannot be negative")
    }
    if (inductance == 0.0) {
        return mutableMapOf<String, Double>("inductance" to (reactance / ((2.0 * PI) * frequency)))
    }
    if (frequency == 0.0) {
        return mutableMapOf<String, Double>("frequency" to (reactance / ((2.0 * PI) * inductance)))
    }
    return mutableMapOf<String, Double>("reactance" to (((2.0 * PI) * frequency) * inductance))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(ind_reactance(0.0, 10000.0, 50.0))
        println(ind_reactance(0.035, 0.0, 50.0))
        println(ind_reactance(0.000035, 1000.0, 0.0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

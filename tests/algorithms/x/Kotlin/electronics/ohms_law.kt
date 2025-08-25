val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/electronics"

fun json(v: Any?) { println(toJson(v)) }

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
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

fun ohms_law(voltage: Double, current: Double, resistance: Double): MutableMap<String, Double> {
    var zeros: Int = (0).toInt()
    if (voltage == 0.0) {
        zeros = zeros + 1
    }
    if (current == 0.0) {
        zeros = zeros + 1
    }
    if (resistance == 0.0) {
        zeros = zeros + 1
    }
    if (zeros != 1) {
        println("One and only one argument must be 0")
        return mutableMapOf<String, Double>()
    }
    if (resistance < 0.0) {
        println("Resistance cannot be negative")
        return mutableMapOf<String, Double>()
    }
    if (voltage == 0.0) {
        return mutableMapOf<String, Double>("voltage" to (current * resistance))
    }
    if (current == 0.0) {
        return mutableMapOf<String, Double>("current" to (voltage / resistance))
    }
    return mutableMapOf<String, Double>("resistance" to (voltage / current))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        json((ohms_law(10.0, 0.0, 5.0)) as Any?)
        json((ohms_law(0.0 - 10.0, 1.0, 0.0)) as Any?)
        json((ohms_law(0.0, 0.0 - 1.5, 2.0)) as Any?)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

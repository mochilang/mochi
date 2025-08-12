fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

var G: Double = 9.80665
fun archimedes_principle(fluid_density: Double, volume: Double, gravity: Double): Double {
    if (fluid_density <= 0.0) {
        panic("Impossible fluid density")
    }
    if (volume <= 0.0) {
        panic("Impossible object volume")
    }
    if (gravity < 0.0) {
        panic("Impossible gravity")
    }
    return (fluid_density * volume) * gravity
}

fun archimedes_principle_default(fluid_density: Double, volume: Double): Double {
    var res: Double = archimedes_principle(fluid_density, volume, G)
    return res
}

fun test_archimedes_principle(): Unit {
    expect(archimedes_principle(500.0, 4.0, 9.8) == 19600.0)
    expect(archimedes_principle(997.0, 0.5, 9.8) == 4885.3)
    var r: Double = archimedes_principle_default(997.0, 0.7)
    var expected: Double = archimedes_principle(997.0, 0.7, G)
    expect(r == expected)
    expect(archimedes_principle(997.0, 0.7, 0.0) == 0.0)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_archimedes_principle()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

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

data class Result(var name: String = "", var value: Double = 0.0)
var r1: Result = shear_stress(25.0, 100.0, 0.0)
fun shear_stress(stress: Double, tangential_force: Double, area: Double): Result {
    var zeros: Int = (0).toInt()
    if (stress == 0.0) {
        zeros = zeros + 1
    }
    if (tangential_force == 0.0) {
        zeros = zeros + 1
    }
    if (area == 0.0) {
        zeros = zeros + 1
    }
    if (zeros != 1) {
        panic("You cannot supply more or less than 2 values")
    } else {
        if (stress < 0.0) {
            panic("Stress cannot be negative")
        } else {
            if (tangential_force < 0.0) {
                panic("Tangential Force cannot be negative")
            } else {
                if (area < 0.0) {
                    panic("Area cannot be negative")
                } else {
                    if (stress == 0.0) {
                        return Result(name = "stress", value = tangential_force / area)
                    } else {
                        if (tangential_force == 0.0) {
                            return Result(name = "tangential_force", value = stress * area)
                        } else {
                            return Result(name = "area", value = tangential_force / stress)
                        }
                    }
                }
            }
        }
    }
}

fun str_result(r: Result): String {
    return ((("Result(name='" + r.name) + "', value=") + r.value.toString()) + ")"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(str_result(r1))
        var r2: Result = shear_stress(0.0, 1600.0, 200.0)
        println(str_result(r2))
        var r3: Result = shear_stress(1000.0, 0.0, 1200.0)
        println(str_result(r3))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

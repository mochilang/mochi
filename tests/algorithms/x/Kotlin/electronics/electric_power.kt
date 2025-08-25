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

data class Result(var name: String = "", var value: Double = 0.0)
var r1: Result = electric_power(0.0, 2.0, 5.0)
fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
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

fun round_to(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    return kotlin.math.floor((x * m) + 0.5) / m
}

fun electric_power(voltage: Double, current: Double, power: Double): Result {
    var zeros: Int = (0).toInt()
    if (voltage == 0.0) {
        zeros = zeros + 1
    }
    if (current == 0.0) {
        zeros = zeros + 1
    }
    if (power == 0.0) {
        zeros = zeros + 1
    }
    if (zeros != 1) {
        panic("Exactly one argument must be 0")
    } else {
        if (power < 0.0) {
            panic("Power cannot be negative in any electrical/electronics system")
        } else {
            if (voltage == 0.0) {
                return Result(name = "voltage", value = power / current)
            } else {
                if (current == 0.0) {
                    return Result(name = "current", value = power / voltage)
                } else {
                    if (power == 0.0) {
                        var p: Double = absf(voltage * current)
                        return Result(name = "power", value = round_to(p, 2))
                    } else {
                        panic("Unhandled case")
                    }
                }
            }
        }
    }
}

fun str_result(r: Result): String {
    return ((("Result(name='" + r.name) + "', value=") + _numToStr(r.value)) + ")"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(str_result(r1))
        var r2: Result = electric_power(2.0, 2.0, 0.0)
        println(str_result(r2))
        var r3: Result = electric_power(0.0 - 2.0, 3.0, 0.0)
        println(str_result(r3))
        var r4: Result = electric_power(2.2, 2.2, 0.0)
        println(str_result(r4))
        var r5: Result = electric_power(2.0, 0.0, 6.0)
        println(str_result(r5))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

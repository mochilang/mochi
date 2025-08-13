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

data class Result(var kind: String = "", var value: Double = 0.0)
var ELECTRON_CHARGE: Double = 0.00000000000000000016021
var r1: Result = electric_conductivity(25.0, 100.0, 0.0)
var r2: Result = electric_conductivity(0.0, 1600.0, 200.0)
var r3: Result = electric_conductivity(1000.0, 0.0, 1200.0)
fun electric_conductivity(conductivity: Double, electron_conc: Double, mobility: Double): Result {
    var zero_count: Int = (0).toInt()
    if (conductivity == 0.0) {
        zero_count = zero_count + 1
    }
    if (electron_conc == 0.0) {
        zero_count = zero_count + 1
    }
    if (mobility == 0.0) {
        zero_count = zero_count + 1
    }
    if (zero_count != 1) {
        panic("You cannot supply more or less than 2 values")
    }
    if (conductivity < 0.0) {
        panic("Conductivity cannot be negative")
    }
    if (electron_conc < 0.0) {
        panic("Electron concentration cannot be negative")
    }
    if (mobility < 0.0) {
        panic("mobility cannot be negative")
    }
    if (conductivity == 0.0) {
        return Result(kind = "conductivity", value = (mobility * electron_conc) * ELECTRON_CHARGE)
    }
    if (electron_conc == 0.0) {
        return Result(kind = "electron_conc", value = conductivity / (mobility * ELECTRON_CHARGE))
    }
    return Result(kind = "mobility", value = conductivity / (electron_conc * ELECTRON_CHARGE))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((r1.kind + " ") + r1.value.toString())
        println((r2.kind + " ") + r2.value.toString())
        println((r3.kind + " ") + r3.value.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

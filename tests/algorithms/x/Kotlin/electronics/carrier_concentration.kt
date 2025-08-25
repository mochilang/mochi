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

data class CarrierResult(var name: String = "", var value: Double = 0.0)
var r1: CarrierResult = carrier_concentration(25.0, 100.0, 0.0)
fun sqrtApprox(x: Double): Double {
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun carrier_concentration(electron_conc: Double, hole_conc: Double, intrinsic_conc: Double): CarrierResult {
    var zero_count: Int = (0).toInt()
    if (electron_conc == 0.0) {
        zero_count = zero_count + 1
    }
    if (hole_conc == 0.0) {
        zero_count = zero_count + 1
    }
    if (intrinsic_conc == 0.0) {
        zero_count = zero_count + 1
    }
    if (zero_count != 1) {
        panic("You cannot supply more or less than 2 values")
    }
    if (electron_conc < 0.0) {
        panic("Electron concentration cannot be negative in a semiconductor")
    }
    if (hole_conc < 0.0) {
        panic("Hole concentration cannot be negative in a semiconductor")
    }
    if (intrinsic_conc < 0.0) {
        panic("Intrinsic concentration cannot be negative in a semiconductor")
    }
    if (electron_conc == 0.0) {
        return CarrierResult(name = "electron_conc", value = (intrinsic_conc * intrinsic_conc) / hole_conc)
    }
    if (hole_conc == 0.0) {
        return CarrierResult(name = "hole_conc", value = (intrinsic_conc * intrinsic_conc) / electron_conc)
    }
    if (intrinsic_conc == 0.0) {
        return CarrierResult(name = "intrinsic_conc", value = sqrtApprox(electron_conc * hole_conc))
    }
    return CarrierResult(name = "", value = 0.0 - 1.0)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((r1.name + ", ") + _numToStr(r1.value))
        var r2: CarrierResult = carrier_concentration(0.0, 1600.0, 200.0)
        println((r2.name + ", ") + _numToStr(r2.value))
        var r3: CarrierResult = carrier_concentration(1000.0, 0.0, 1200.0)
        println((r3.name + ", ") + _numToStr(r3.value))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

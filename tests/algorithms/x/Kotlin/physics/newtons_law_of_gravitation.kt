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
var GRAVITATIONAL_CONSTANT: Double = 0.000000000066743
var r1: Result = gravitational_law(0.0, 5.0, 10.0, 20.0)
var r2: Result = gravitational_law(7367.382, 0.0, 74.0, 3048.0)
var r3: Result = gravitational_law(100.0, 5.0, 0.0, 3.0)
var r4: Result = gravitational_law(100.0, 5.0, 10.0, 0.0)
fun sqrtApprox(x: Double): Double {
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun gravitational_law(force: Double, mass_1: Double, mass_2: Double, distance: Double): Result {
    var zero_count: Int = (0).toInt()
    if (force == 0.0) {
        zero_count = zero_count + 1
    }
    if (mass_1 == 0.0) {
        zero_count = zero_count + 1
    }
    if (mass_2 == 0.0) {
        zero_count = zero_count + 1
    }
    if (distance == 0.0) {
        zero_count = zero_count + 1
    }
    if (zero_count != 1) {
        panic("One and only one argument must be 0")
    }
    if (force < 0.0) {
        panic("Gravitational force can not be negative")
    }
    if (distance < 0.0) {
        panic("Distance can not be negative")
    }
    if (mass_1 < 0.0) {
        panic("Mass can not be negative")
    }
    if (mass_2 < 0.0) {
        panic("Mass can not be negative")
    }
    var product_of_mass: Double = mass_1 * mass_2
    if (force == 0.0) {
        var f: Double = (GRAVITATIONAL_CONSTANT * product_of_mass) / (distance * distance)
        return Result(kind = "force", value = f)
    }
    if (mass_1 == 0.0) {
        var m1: Double = (force * (distance * distance)) / (GRAVITATIONAL_CONSTANT * mass_2)
        return Result(kind = "mass_1", value = m1)
    }
    if (mass_2 == 0.0) {
        var m2: Double = (force * (distance * distance)) / (GRAVITATIONAL_CONSTANT * mass_1)
        return Result(kind = "mass_2", value = m2)
    }
    var d: Double = sqrtApprox((GRAVITATIONAL_CONSTANT * product_of_mass) / force)
    return Result(kind = "distance", value = d)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((r1.kind + " ") + r1.value.toString())
        println((r2.kind + " ") + r2.value.toString())
        println((r3.kind + " ") + r3.value.toString())
        println((r4.kind + " ") + r4.value.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

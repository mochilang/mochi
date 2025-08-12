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
var REDUCED_PLANCK_CONSTANT: Double = 0.0000000000000000000000000000000001054571817
var SPEED_OF_LIGHT: Double = 300000000.0
fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 100) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun casimir_force(force: Double, area: Double, distance: Double): MutableMap<String, Double> {
    var zero_count: Int = (0).toInt()
    if (force == 0.0) {
        zero_count = zero_count + 1
    }
    if (area == 0.0) {
        zero_count = zero_count + 1
    }
    if (distance == 0.0) {
        zero_count = zero_count + 1
    }
    if (zero_count != 1) {
        panic("One and only one argument must be 0")
    }
    if (force < 0.0) {
        panic("Magnitude of force can not be negative")
    }
    if (distance < 0.0) {
        panic("Distance can not be negative")
    }
    if (area < 0.0) {
        panic("Area can not be negative")
    }
    if (force == 0.0) {
        var num: Double = (((REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT) * PI) * PI) * area
        var den: Double = (((240.0 * distance) * distance) * distance) * distance
        var f: Double = num / den
        return mutableMapOf<String, Double>("force" to (f))
    }
    if (area == 0.0) {
        var num: Double = ((((240.0 * force) * distance) * distance) * distance) * distance
        var den: Double = ((REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT) * PI) * PI
        var a: Double = num / den
        return mutableMapOf<String, Double>("area" to (a))
    }
    var num: Double = (((REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT) * PI) * PI) * area
    var den: Double = 240.0 * force
    var inner: Double = num / den
    var d: Double = sqrtApprox(sqrtApprox(inner))
    return mutableMapOf<String, Double>("distance" to (d))
}

fun user_main(): Unit {
    println(casimir_force(0.0, 4.0, 0.03).toString())
    println(casimir_force(0.0000000002635, 0.0023, 0.0).toString())
    println(casimir_force(0.000000000000000002737, 0.0, 0.0023746).toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

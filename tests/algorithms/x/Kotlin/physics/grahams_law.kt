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

fun to_float(x: Int): Double {
    return x * 1.0
}

fun round6(x: Double): Double {
    var factor: Double = 1000000.0
    return to_float((((x * factor) + 0.5).toInt())) / factor
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun validate(values: MutableList<Double>): Boolean {
    if (values.size == 0) {
        return false
    }
    var i: Int = (0).toInt()
    while (i < values.size) {
        if (values[i]!! <= 0.0) {
            return false
        }
        i = i + 1
    }
    return true
}

fun effusion_ratio(m1: Double, m2: Double): Double {
    if (!validate(mutableListOf(m1, m2))) {
        println("ValueError: Molar mass values must greater than 0.")
        return 0.0
    }
    return round6(sqrtApprox(m2 / m1))
}

fun first_effusion_rate(rate: Double, m1: Double, m2: Double): Double {
    if (!validate(mutableListOf(rate, m1, m2))) {
        println("ValueError: Molar mass and effusion rate values must greater than 0.")
        return 0.0
    }
    return round6(rate * sqrtApprox(m2 / m1))
}

fun second_effusion_rate(rate: Double, m1: Double, m2: Double): Double {
    if (!validate(mutableListOf(rate, m1, m2))) {
        println("ValueError: Molar mass and effusion rate values must greater than 0.")
        return 0.0
    }
    return round6(rate / sqrtApprox(m2 / m1))
}

fun first_molar_mass(mass: Double, r1: Double, r2: Double): Double {
    if (!validate(mutableListOf(mass, r1, r2))) {
        println("ValueError: Molar mass and effusion rate values must greater than 0.")
        return 0.0
    }
    var ratio: Double = r1 / r2
    return round6(mass / (ratio * ratio))
}

fun second_molar_mass(mass: Double, r1: Double, r2: Double): Double {
    if (!validate(mutableListOf(mass, r1, r2))) {
        println("ValueError: Molar mass and effusion rate values must greater than 0.")
        return 0.0
    }
    var ratio: Double = r1 / r2
    return round6((ratio * ratio) / mass)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(effusion_ratio(2.016, 4.002))
        println(first_effusion_rate(1.0, 2.016, 4.002))
        println(second_effusion_rate(1.0, 2.016, 4.002))
        println(first_molar_mass(2.0, 1.408943, 0.709752))
        println(second_molar_mass(2.0, 1.408943, 0.709752))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}

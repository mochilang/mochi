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

fun floor(x: Double): Double {
    var i: Int = (x.toInt())
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun pow10(n: Int): Double {
    var p: Double = 1.0
    var i: Int = 0
    while (i < n) {
        p = p * 10.0
        i = i + 1
    }
    return p
}

fun round_to(x: Double, ndigits: Int): Double {
    var m: Double = pow10(ndigits)
    return floor((x * m) + 0.5) / m
}

fun celsius_to_fahrenheit(c: Double, ndigits: Int): Double {
    return round_to(((c * 9.0) / 5.0) + 32.0, ndigits)
}

fun celsius_to_kelvin(c: Double, ndigits: Int): Double {
    return round_to(c + 273.15, ndigits)
}

fun celsius_to_rankine(c: Double, ndigits: Int): Double {
    return round_to(((c * 9.0) / 5.0) + 491.67, ndigits)
}

fun fahrenheit_to_celsius(f: Double, ndigits: Int): Double {
    return round_to(((f - 32.0) * 5.0) / 9.0, ndigits)
}

fun fahrenheit_to_kelvin(f: Double, ndigits: Int): Double {
    return round_to((((f - 32.0) * 5.0) / 9.0) + 273.15, ndigits)
}

fun fahrenheit_to_rankine(f: Double, ndigits: Int): Double {
    return round_to(f + 459.67, ndigits)
}

fun kelvin_to_celsius(k: Double, ndigits: Int): Double {
    return round_to(k - 273.15, ndigits)
}

fun kelvin_to_fahrenheit(k: Double, ndigits: Int): Double {
    return round_to((((k - 273.15) * 9.0) / 5.0) + 32.0, ndigits)
}

fun kelvin_to_rankine(k: Double, ndigits: Int): Double {
    return round_to((k * 9.0) / 5.0, ndigits)
}

fun rankine_to_celsius(r: Double, ndigits: Int): Double {
    return round_to(((r - 491.67) * 5.0) / 9.0, ndigits)
}

fun rankine_to_fahrenheit(r: Double, ndigits: Int): Double {
    return round_to(r - 459.67, ndigits)
}

fun rankine_to_kelvin(r: Double, ndigits: Int): Double {
    return round_to((r * 5.0) / 9.0, ndigits)
}

fun reaumur_to_kelvin(r: Double, ndigits: Int): Double {
    return round_to((r * 1.25) + 273.15, ndigits)
}

fun reaumur_to_fahrenheit(r: Double, ndigits: Int): Double {
    return round_to((r * 2.25) + 32.0, ndigits)
}

fun reaumur_to_celsius(r: Double, ndigits: Int): Double {
    return round_to(r * 1.25, ndigits)
}

fun reaumur_to_rankine(r: Double, ndigits: Int): Double {
    return round_to(((r * 2.25) + 32.0) + 459.67, ndigits)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(celsius_to_fahrenheit(0.0, 2))
        println(celsius_to_kelvin(0.0, 2))
        println(celsius_to_rankine(0.0, 2))
        println(fahrenheit_to_celsius(32.0, 2))
        println(fahrenheit_to_kelvin(32.0, 2))
        println(fahrenheit_to_rankine(32.0, 2))
        println(kelvin_to_celsius(273.15, 2))
        println(kelvin_to_fahrenheit(273.15, 2))
        println(kelvin_to_rankine(273.15, 2))
        println(rankine_to_celsius(491.67, 2))
        println(rankine_to_fahrenheit(491.67, 2))
        println(rankine_to_kelvin(491.67, 2))
        println(reaumur_to_kelvin(80.0, 2))
        println(reaumur_to_fahrenheit(80.0, 2))
        println(reaumur_to_celsius(80.0, 2))
        println(reaumur_to_rankine(80.0, 2))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
